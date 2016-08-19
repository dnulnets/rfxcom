{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import qualified System.IO                        as SIO

import           Control.Concurrent               (ThreadId (..), killThread)
import           Control.Concurrent.Chan          (Chan, newChan, readChan,
                                                   writeChan)
import           Control.Concurrent.MVar          (MVar, newEmptyMVar, newMVar,
                                                   putMVar, takeMVar)

import           Control.Exception
import           Control.Monad
import           Control.Monad.Managed            (Managed, managed, runManaged)
import           Control.Monad.Reader             (MonadReader (..),
                                                   ReaderT (..), ask,
                                                   runReaderT)
import           Control.Monad.State              (MonadState (..), StateT (..),
                                                   evalStateT, get, put)

import           Pipes

import qualified Pipes.Binary                     as PB (DecodingError (..),
                                                         decodeGet)
import qualified Pipes.ByteString                 as PBS (fromLazy, hGetSome,
                                                          toHandle)
import qualified Pipes.Parse                      as PP
import qualified Pipes.Prelude                    as P (mapM, mapM_, repeatM)
import           Pipes.Safe

import           Data.ByteString                  (ByteString)
import           Data.Word                        (Word8)

--
-- Internal import section
--
import qualified RFXCom.Control.RFXComWriter      as RFXComW (Handle (..),
                                                              Message (..))
import           RFXCom.Message.Base
import           RFXCom.Message.Decoder           (msgParser)
import qualified RFXCom.Message.InterfaceControl  as IC (Body (..),
                                                         Command (..))
import qualified RFXCom.Message.InterfaceResponse as IR (Body (..))
import           RFXCom.System.Concurrent         (executeAfterDelay, forkChild,
                                                   waitForChildren)
import           RFXCom.System.Exception          (ResourceException (..))
import qualified RFXCom.System.Log                as Log (Handle (..), LoggerT,
                                                          MonadLogger(..),
                                                          runLoggerT, _debug,
                                                          _error, _info,
                                                          _warning)

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
data Environment = Environment {
  loggerH   :: Log.Handle        -- ^The handle to the logger service
  , writerH :: RFXComW.Handle  -- ^The handle to the RFXCom Device Writer process
  , mvar    :: MVar RFXCom.Control.RFXComMaster.Message}   -- ^The communcation mvar


-- |The state machines states
data MachineState = Start                       -- ^The state machine has been started
                  | ResetTheDevice              -- ^Reset the RFXCom device
                  | WaitForStatusFromTheDevice  -- ^Wait for status response from the RFXCom device affter a get status command
                  | WaitForStartFromTheDevice   -- ^Wait for status response from the RFXCom device after a start device command
                  | WaitForMessage              -- ^Wait for sensor readingings from the RFXCom device
                  | Stopped                     -- ^The state machine has been stopped
                  | Error                       --  The state machine is in error state
                  deriving (Eq,Show)

-- |The state of the thread
data State = State {
  machineState :: MachineState,
  timer        :: Maybe ThreadId
  } deriving (Show)

-- |Performs an IO action with the RFXCom writer process. Note that for each withHandle
-- a new RFXCom writer communication thread will be started.
withHandle::Config          -- ^The configuration of the master threads
          ->Log.Handle      -- ^The handle to the Logger service
          ->RFXComW.Handle  -- ^The handle to the RFXCom Writer service
          ->(Handle->IO a)  -- ^The IO action to perform
          ->IO a
withHandle config loggerH writerH io = do
  env <- Environment loggerH writerH <$> newEmptyMVar
  tid <- forkChild $ masterThread env
  x <- io $ Handle { send = sendToMaster env}
  s <- stopMasterThread env
  return x

--
-- RFXCom Master Monad
--

-- |The monad that the RFXCom Master executes under
newtype RFXComMaster m a = RFXComMaster (ReaderT Environment (StateT State (Log.LoggerT m)) a)
  deriving (Functor, Applicative, Monad, MonadReader Environment, MonadIO,
            Log.MonadLogger, MonadState State)


-- |The lift for the RFXCom Master monad
instance MonadTrans RFXComMaster where
  lift m = RFXComMaster $ lift $ lift $ lift m

-- |Injects the environment and runs the computations in the RFXCom Master monad
runRFXComMaster::(Monad m) => RFXComMaster m a -- ^The RFXCom Master monad
               -> Environment     -- ^The environment that the monad should be evaluated under
               -> State           -- ^The initial state of the RFXCom Writer monad
               ->m a              -- ^The result
runRFXComMaster (RFXComMaster m) env state = Log.runLoggerT (evalStateT (runReaderT m env) state) (loggerH env)

--
-- The serial port writer functions
--

-- |Sends a message to the master thread.
sendToMaster::Environment                         -- ^The internal handle to the master thread
            ->RFXCom.Control.RFXComMaster.Message -- ^Tĥe message to send
            ->IO ()
sendToMaster env msg = do
  putMVar (mvar env) msg

-- |Stops the master thread by sending a Stop message. This function do not return until the thread
-- has acknowledged the Stop message.
stopMasterThread::Environment -- ^The internal handle to the master thread
                -> IO ()
stopMasterThread ih = do
  s<-newEmptyMVar
  sendToMaster ih $ Stop s
  takeMVar s

-- |Schedules a timeout to be sent to the master thread.
scheduleTimeout::(Monad m, MonadIO m)=>Int -- ^The timeout in microseconds
               -> RFXComMaster m () 
scheduleTimeout t = do
  env <- ask
  state <- get
  tid <- liftIO $ executeAfterDelay t $ sendToMaster env Timeout
  put $ state {timer = Just tid}
  Log.info $ "RFXCom.Control.RFXComMaster.scheduleTimeout: Scheduling a timeout for " ++ show t ++ " " ++ show tid
  return ()

-- |Cancels the timeout
cancelTimeout::(Monad m, MonadIO m)=>RFXComMaster m ()
cancelTimeout = do
  state <- get
  Log.info $ "RFXCom.Control.RFXComMaster.scheduleTimeout: Cancelling current timeout for " ++ show (timer state)
  liftIO $ maybe (return ()) (killThread) (timer state)
  put state {timer = Nothing}


-- |Executes the statemachine based on the current state and incoming message. This function
-- will return with the next state
executeStateMachine::(Monad m, MonadIO m)=>MachineState    -- ^The current state of the machine
                   ->RFXCom.Control.RFXComMaster.Message -- ^The incoming message to the state machine
                   ->RFXComMaster m ()
--
-- State: Start Signal: anything
--
-- Flush the serial port and send the Get Status command to the RFXCom device
--
executeStateMachine Start _ = do
  env <- ask
  state <- get
  Log.info $ "RFXCom.Control.RFXComMaster.executeStateMachine: Started"
  liftIO $ putStrLn "Resetting the RFXComDevice"
  liftIO $ RFXComW.send (writerH env) $ RFXComW.Flush
  liftIO $ RFXComW.send (writerH env) $ RFXComW.Message $ InterfaceControl IC.Body {IC._cmnd = IC.Reset}
  scheduleTimeout 1000000
  put state {machineState = ResetTheDevice}
  return ()

--
-- State: ResetTheDevice Signal: Timeout
--
-- Flush the serial port and send the Get Status command to the RFXCom device
--
executeStateMachine ResetTheDevice Timeout = do
  env <- ask
  state <- get
  Log.info $ "RFXCom.Control.RFXComMaster.executeStateMachine: ResetTheDevice got Timeout (" ++ show (timer state) ++ ")"
  liftIO $ RFXComW.send (writerH env) RFXComW.Flush
  liftIO $ putStrLn "RFXCom Device is reset"
  liftIO $ RFXComW.send (writerH env) $ RFXComW.Message $ InterfaceControl IC.Body {IC._cmnd = IC.GetStatus}
  scheduleTimeout 1000000
  put state {machineState = WaitForStatusFromTheDevice}
  return ()

--
-- State: WaitForStatusFromTheDevice Signal: Message from RFXComDevice InterfaceControlResponse
--
-- We got what we wanted, so cancel the current timer and send a start receiver command to the RFXCom device
-- and reschedule a new timer for the response.
--
executeStateMachine WaitForStatusFromTheDevice msg@(RFXCom.Control.RFXComMaster.Message (InterfaceResponse IR.Body {IR._cmnd=2})) = do
  env <- ask
  state <- get  
  Log.info $ "RFXCom.Control.RFXComMaster.executeStateMachine: WaitForStatusFromTheDevice got Message (" ++ show (timer state) ++ ")"
  Log.info $ "RFXCom.Control.RFXComMaster.executeStateMachine: " ++ show msg
  cancelTimeout
  liftIO $ putStrLn "RFXCom Device status is ok"
  liftIO $ RFXComW.send (writerH env) $ RFXComW.Message $ InterfaceControl IC.Body {IC._cmnd = IC.Start}
  scheduleTimeout 1000000
  put state {machineState = WaitForStartFromTheDevice}
  return ()

--
-- State: WaitForStatusFromTheDevice Signal: Any message from RFXComDevice
--
-- Ignore any other message that is not a response to the get status message. Just continue waiting.
--
executeStateMachine WaitForStatusFromTheDevice msg@(RFXCom.Control.RFXComMaster.Message _ ) = do
  state <- get  
  Log.info $ "RFXCom.Control.RFXComMaster.executeStateMachine: WaitForStatusFromTheDevice ignoring wrong message (" ++ show (timer state) ++ ")"
  Log.info $ "RFXCom.Control.RFXComMaster.executeStateMachine: " ++ show msg
  return ()

--
-- State: WaitForStatusFromTheDevice Signal: Timeout
--
-- We did not get the response we wanted from the RFXCom device within the timeout, so we need to reset the
-- device again and retry this
--
executeStateMachine WaitForStatusFromTheDevice Timeout = do
  env <- ask
  state <- get  
  Log.info $ "RFXCom.Control.RFXComMaster.executeStateMachine: WaitForStatusFromTheDevice got Timeout (" ++ show (timer state) ++ ")"
  liftIO $ putStrLn "RFXCom Device timeout, resetting"
  liftIO $ RFXComW.send (writerH env) $ RFXComW.Message $ InterfaceControl IC.Body {IC._cmnd = IC.Reset}
  scheduleTimeout 1000000
  put state {machineState = ResetTheDevice}
  return ()

--
-- State: WaitForStartFromDevice Signal: Message from RFXComDevice InterfaceControlResponse
--
-- We got a start acknowledge from the RFXCom device, so we are now good to go. Start waiting for messages from
-- the RFXCom device or the MQTT broker.
--
executeStateMachine WaitForStartFromTheDevice  msg@(RFXCom.Control.RFXComMaster.Message (InterfaceResponse IR.Body {IR._cmnd=7})) = do
  env <- ask
  state <- get  
  Log.info $ "RFXCom.Control.RFXComMaster.executeStateMachine: WaitForStartFomTheDevice got Message (" ++ show (timer state) ++ ")"
  Log.info $ "RFXCom.Control.RFXComMaster.executeStateMachine: " ++ show msg
  liftIO $ putStrLn "RFXCom Device started"
  cancelTimeout
  put state {machineState = WaitForMessage, timer = Nothing}
  return ()

executeStateMachine WaitForStartFromTheDevice  msg@(RFXCom.Control.RFXComMaster.Message _) = do
  env <- ask
  state <- get  
  Log.info $ "RFXCom.Control.RFXComMaster.executeStateMachine: WaitForStartFomTheDevice ignoring wrong Message (" ++ show (timer state) ++ ")"
  Log.info $ "RFXCom.Control.RFXComMaster.executeStateMachine: " ++ show msg
  return ()

executeStateMachine WaitForStartFromTheDevice Timeout = do
  env <- ask
  state <- get  
  Log.info $ "RFXCom.Control.RFXComMaster.executeStateMachine: WaitForStartFromTheDevice got Timeout (" ++ show (timer state) ++ ")"
  liftIO $ RFXComW.send (writerH env) $ RFXComW.Message $ InterfaceControl IC.Body {IC._cmnd = IC.Reset}
  liftIO $ putStrLn "RFXCom Device start timeout, resetting"
  scheduleTimeout 1000000
  put state {machineState = ResetTheDevice}
  return ()

--
-- State: WaitForMessage Signal: Any message
--
-- Just print the message for now
--
executeStateMachine WaitForMessage msg = do
  env <- ask
  state <- get  
  Log.info $ "RFXCom.Control.RFXComMaster.executeStateMachine: WaitForMessages got Message"
  Log.info $ "RFXCom.Control.RFXComMaster.executeStateMachine: " ++ show msg
  return ()

--
-- State: Any Signal: Any
--
-- This is the catch all state handler, i.e. if we missged something above.
--
executeStateMachine st signal = do
  env <- ask
  state <- get  
  Log.info $ "Unhandled state or signal, State=" ++ show state ++ ", Signal=" ++ show signal
  return ()


-- |The writer threads that controls messages to and from the RFXCom device.
processMasterHandler::(Monad m, MonadIO m)=>RFXComMaster m ()
processMasterHandler = do
  env <- ask
  state <- get
  if (machineState state) ==  Start
    then do
      executeStateMachine Start Timeout
      processMasterHandler
    else do
      cmd <- liftIO $ takeMVar $ mvar env
      case cmd of
        Stop s -> do
          liftIO $ putMVar s ()
          put state {machineState = Stopped}
          return ()
        _ -> do
          executeStateMachine (machineState state) cmd
          processMasterHandler


-- |Spawns off the master handler
masterThread::Environment -- ^The environemnt that we must execute under
            ->IO ()
masterThread env = do
  
  Log._info (loggerH env) "RFXCom.Control.RFXComMaster.masterThread: Master thread is up and running"

  -- Run the RFXCom Master
  runRFXComMaster processMasterHandler env (State Start Nothing)
  
