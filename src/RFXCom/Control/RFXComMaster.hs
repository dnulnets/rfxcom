-- |This module holds the master control process for the RFXCom device
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.Control.RFXComMaster (
  Handle(..),
  Config(..),
  RFXCom.Control.RFXComMaster.Message(..),
  defaultConfig,
  withHandle,
  ) where

--
-- Import section
--
import qualified System.IO                  as SIO

import           Control.Concurrent.Chan    (Chan, newChan, readChan, writeChan)
import           Control.Concurrent.MVar    (MVar, newEmptyMVar, newMVar,
                                             putMVar, takeMVar)
import           Control.Concurrent (ThreadId(..), killThread)

import           Control.Exception
import           Control.Monad
import           Control.Monad.Managed      (Managed, managed, runManaged)

import           Pipes

import qualified Pipes.Binary               as PB (DecodingError (..),
                                                   decodeGet)
import qualified Pipes.ByteString           as PBS (fromLazy, hGetSome,
                                                    toHandle)
import qualified Pipes.Parse                as PP
import qualified Pipes.Prelude              as P (mapM_, repeatM, mapM)
import           Pipes.Safe

import           Data.ByteString            (ByteString)
import           Data.Word                  (Word8)

--
-- Internal import section
--
import           RFXCom.Message.Base
import qualified          RFXCom.Message.InterfaceControl as IC
import qualified          RFXCom.Message.InterfaceResponse as IR
import           RFXCom.Message.Decoder     (msgParser)
import           RFXCom.System.Concurrent   (forkChild, waitForChildren, executeAfterDelay)
import           RFXCom.System.Exception    (ResourceException (..))
import qualified RFXCom.Control.RFXComWriter as RFXComW (Handle(..), Message(..))
import qualified RFXCom.System.Log          as Log (Handle (..), debug, error,
                                                    info, warning)

-- |The configuration of the RFXCom master process settings
data Config = Config


-- |Default RFXCom master process setting
defaultConfig = Config


-- |Ugly one, ust to get deriving Show to work. I do not really need to show the MVar.
instance Show (MVar a) where
  show m = "MVar"
  
-- |The messages to the master thread
data Message = Message RFXCom.Message.Base.Message -- ^Any incoming message from the RFXCom device
             | Timeout          -- ^The timeout message, acts as a heartbeat
             | Stop (MVar ())   -- ^Shuts down the mater threads
             deriving (Show)

-- |The service handle to the communcation processes.
data Handle = Handle {
  send::RFXCom.Control.RFXComMaster.Message->IO () -- ^The sender function of commands to the thread
  }

-- |The internal service handle to the communcation processes.
data IHandle = IHandle {
  loggerH::Log.Handle        -- ^The handle to the logger service
  , writerH::RFXComW.Handle  -- ^The handle to the RFXCom Device Writer process
  , mvar::MVar RFXCom.Control.RFXComMaster.Message}   -- ^The communcation mvar


-- |The state machines states
data State = ResetTheDevice
           | WaitForStatusFromTheDevice
           | WaitForStartFromTheDevice
           | WaitForMessage
           | Error             
           deriving (Show)

-- |Performs an IO action with the RFXCom writer process. Note that for each withHandle
-- a new RFXCom writer communication thread will be started.
withHandle::Config          -- ^The configuration of the master threads
          ->Log.Handle      -- ^The handle to the Logger service
          ->RFXComW.Handle  -- ^The handle to the RFXCom Writer service
          ->(Handle->IO a)  -- ^The IO action to perform
          ->IO a
withHandle config loggerH writerH io = do
  ih <- IHandle loggerH writerH <$> newEmptyMVar
  tid <- forkChild $ masterThread ih
  x <- io $ Handle { send = sendToMaster ih}
  s <- stopMasterThread ih
  return x


--
-- The serial port writer functions
--

-- |Sends a message to the master thread.
sendToMaster::IHandle                             -- ^The internal handle to the master thread
            ->RFXCom.Control.RFXComMaster.Message -- ^Tĥe message to send
            ->IO ()
sendToMaster ih msg = do
  putMVar (mvar ih) msg

-- |Stops the master thread by sending a Stop message. This function do not return until the thread
-- has acknowledged the Stop message.
stopMasterThread::IHandle -- ^The internal handle to the master thread
                -> IO ()
stopMasterThread ih = do
  s<-newEmptyMVar
  sendToMaster ih $ Stop s
  takeMVar s

-- |Schedules a timeout to be sent to the master thread.
scheduleTimeout::IHandle -- ^The internal handle to the master thread
               ->Int     -- ^The timeout in microseconds
               -> IO ThreadId -- ^The threadid of the thread doing the waiting and execution
scheduleTimeout ih t = do
  tid <- executeAfterDelay t $ sendToMaster ih Timeout
  Log.info (loggerH ih) $ "RFXCom.Control.RFXComMaster.scheduleTimeout: Scheduling a timeout for " ++ show t ++ " " ++ show tid
  return tid

-- |Cancels the timeout
cancelTimeout::IHandle   -- ^Internal handle to the master thread
             ->ThreadId  -- ^The thread id of the timer thread that is going to be cancelled
             ->IO ()
cancelTimeout ih tid = do
  Log.info (loggerH ih) $ "RFXCom.Control.RFXComMaster.scheduleTimeout: Cancelling current timeout for " ++ show tid
  killThread tid


-- |Executes the statemachine based on the current state and incoming message. This function
-- will return with the next state
executeStateMachine::IHandle -- ^The internal handle to the master thread
                   ->Maybe ThreadId -- ^Current running timer
                   ->State   -- ^The current state of the machine
                   ->RFXCom.Control.RFXComMaster.Message -- ^The incoming message to the state machine
                   ->IO (State,Maybe ThreadId) -- ^The new state and the current timer
--
-- State: ResetTheDevice Signal: Timeout
--
-- Flush the serial port and send the Get Status command to the RFXCom device
--
executeStateMachine ih tmr ResetTheDevice Timeout = do
  Log.info (loggerH ih) $ "RFXCom.Control.RFXComMaster.executeStateMachine: ResetTheDevice got Timeout"
  RFXComW.send (writerH ih) RFXComW.Flush
  RFXComW.send (writerH ih) $ RFXComW.Message $ InterfaceControl IC.InterfaceControlBody {IC._cmnd = IC.GetStatus}
  newtmr <- scheduleTimeout ih 5000000
  return (WaitForStatusFromTheDevice, Just newtmr)

--
-- State: WaitForStatusFromTheDevice Signal: Message from RFXComDevice InterfaceControlResponse
--
-- We got what we wanted, so cancel the current timer and send a start receiver command to the RFXCom device
-- and reschedule a new timer for the response.
--
executeStateMachine ih tmr WaitForStatusFromTheDevice msg@(RFXCom.Control.RFXComMaster.Message (InterfaceResponse body)) = do
  Log.info (loggerH ih) $ "RFXCom.Control.RFXComMaster.executeStateMachine: WaitForStatusFromTheDevice got Message"
  Log.info (loggerH ih) $ "RFXCom.Control.RFXComMaster.executeStateMachine: " ++ show msg  
  pure $ (cancelTimeout ih) <$> tmr
  RFXComW.send (writerH ih) $ RFXComW.Message $ InterfaceControl IC.InterfaceControlBody {IC._cmnd = IC.Start}
  newtmr <- scheduleTimeout ih 5000000
  return (WaitForStartFromTheDevice, Just newtmr)
  
--
-- State: WaitForStatusFromTheDevice Signal: Any message from RFXComDevice
--
-- Ignore any other message that is not a response to the get status message. Just continue waiting.
--
executeStateMachine ih tmr WaitForStatusFromTheDevice msg@(RFXCom.Control.RFXComMaster.Message _ ) = do
  Log.info (loggerH ih) $ "RFXCom.Control.RFXComMaster.executeStateMachine: WaitForStatusFromTheDevice ignoring wrong message"
  Log.info (loggerH ih) $ "RFXCom.Control.RFXComMaster.executeStateMachine: " ++ show msg
  return (WaitForStatusFromTheDevice, tmr)

--
-- State: WaitForStatusFromTheDevice Signal: Timeout
--
-- We did not get the response we wanted from the RFXCom device within the timeout, so we need to reset the
-- device again and retry this
--
executeStateMachine ih tmr WaitForStatusFromTheDevice Timeout = do
  Log.info (loggerH ih) $ "RFXCom.Control.RFXComMaster.executeStateMachine: WaitForStatusFromTheDevice got Timeout"
  RFXComW.send (writerH ih) $ RFXComW.Message $ InterfaceControl IC.InterfaceControlBody {IC._cmnd = IC.Reset}
  newtmr <- scheduleTimeout ih 5000000  
  return (ResetTheDevice, Just newtmr)

--
-- State: WaitForStartFromDevice Signal: Message from RFXComDevice InterfaceControlResponse
--
-- We got a start acknowledge from the RFXCom device, so we are now good to go. Start waiting for messages from
-- the RFXCom device or the MQTT broker.
--
executeStateMachine ih tmr WaitForStartFromTheDevice  msg@(RFXCom.Control.RFXComMaster.Message (InterfaceResponse body)) = do
  Log.info (loggerH ih) $ "RFXCom.Control.RFXComMaster.executeStateMachine: WaitForStartFomTheDevice got Message"
  Log.info (loggerH ih) $ "RFXCom.Control.RFXComMaster.executeStateMachine: " ++ show msg
  pure $ (cancelTimeout ih) <$> tmr
  return (WaitForMessage, Nothing)

executeStateMachine ih tmr WaitForStartFromTheDevice  msg@(RFXCom.Control.RFXComMaster.Message _) = do
  Log.info (loggerH ih) $ "RFXCom.Control.RFXComMaster.executeStateMachine: WaitForStartFomTheDevice ignoring wrong Message"
  Log.info (loggerH ih) $ "RFXCom.Control.RFXComMaster.executeStateMachine: " ++ show msg
  return (WaitForStartFromTheDevice, tmr)  

executeStateMachine ih tmr WaitForStartFromTheDevice Timeout = do
  Log.info (loggerH ih) $ "RFXCom.Control.RFXComMaster.executeStateMachine: WaitForStartFromTheDevice got Timeout"
  RFXComW.send (writerH ih) $ RFXComW.Message $ InterfaceControl IC.InterfaceControlBody {IC._cmnd = IC.Reset}
  newtmr <- scheduleTimeout ih 5000000
  return (ResetTheDevice, Just newtmr)

--
-- State: WaitForMessage Signal: Any message
--
-- Just print the message for now
--
executeStateMachine ih tmr WaitForMessage msg = do
  Log.info (loggerH ih) $ "RFXCom.Control.RFXComMaster.executeStateMachine: WaitForMessages got Message"  
  Log.info (loggerH ih) $ "RFXCom.Control.RFXComMaster.executeStateMachine: " ++ show msg
  return (WaitForMessage, tmr)

--
-- State: Any Signal: Any
--
-- This is the catch all state handler, i.e. if we missged something above.
--
executeStateMachine ih tmr state signal = do
  Log.info (loggerH ih) $ "Unhandled state or signal, State=" ++ show state ++ ", Signal=" ++ show signal
  return (state, tmr)


-- |The writer threads that controls messages to and from the RFXCom device.
masterThread::IHandle -- ^The internal handle to the master threads
            ->IO ()
masterThread ih = do
  Log.info (loggerH ih) "RFXCom.Control.RFXComMaster.masterThread: Master thread is up and running"

  --
  -- Send the reset command to the RFXCom device
  --
  RFXComW.send (writerH ih) RFXComW.Flush
  RFXComW.send (writerH ih) $ RFXComW.Message $ InterfaceControl IC.InterfaceControlBody {IC._cmnd = IC.Reset}
  scheduleTimeout ih 5000000

  --
  -- Start the command receiving loop and state machine
  --
  loop ResetTheDevice Nothing
  where
    loop state tmr = do 
      cmd <- takeMVar $ mvar ih
      case cmd of
        Stop s -> do
          putMVar s ()
          return ()
        _ -> do
          (newstate, newtmr) <- executeStateMachine ih tmr state cmd
          loop newstate newtmr
      putStrLn ""

        
