-- |This module holds the master control process for the RFXCom device
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.Control.RFXComMaster (
  Handle(..),
  Config(..),
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
import           RFXCom.Message.Base        (Message)
import           RFXCom.Message.Decoder     (msgParser)
import           RFXCom.System.Concurrent   (forkChild, waitForChildren)
import           RFXCom.System.Exception    (ResourceException (..))
import qualified RFXCom.Control.RFXComWriter as RFXComW (Handle(..))
import qualified RFXCom.System.Log          as Log (Handle (..), debug, error,
                                                    info, warning)

-- |The configuration of the RFXCom master process settings
data Config = Config


-- |Default RFXCom master process setting
defaultConfig = Config


-- |The messages to the master thread
data Message = Timeout          -- ^The timeout message, acts as a heartbeat
             | Stop (MVar ())   -- ^Shuts down the mater threads


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
  x <- io $ Handle { send = sendToMasterThread ih}
  s <- stopMasterThread ih
  return x


--
-- The serial port writer functions
--

-- |Sends a message to the master thread.
sendToMasterThread::IHandle                             -- ^The internal handle to the master thread
                  ->RFXCom.Control.RFXComMaster.Message -- ^Tĥe message to send
                  ->IO ()
sendToMasterThread ih msg = do
  putMVar (mvar ih) msg

-- |Stops the master thread by sending a Stop message. This function do not return until the thread
-- has acknowledged the Stop message.
stopMasterThread::IHandle -- ^The internal handle to the master thread
                -> IO ()
stopMasterThread ih = do
  s<-newEmptyMVar
  sendToMasterThread ih $ Stop s
  takeMVar s

-- |Sends a timeout to the master thread.
timeoutMasterThread::IHandle
                   -> IO ()
timeoutMasterThread ih = sendToMasterThread ih Timeout                   

resetTheDevice::IHandle->RFXCom.Control.RFXComMaster.Message->IO State
resetTheDevice ih msg = do
  return ResetTheDevice
  
-- |The writer threads that controls messages to and from the RFXCom device.
masterThread::IHandle -- ^The internal handle to the master threads
            ->IO ()
masterThread ih = do
  Log.info (loggerH ih) "RFXCom.Control.RFXComMaster.masterThread: Master thread is up and running"
  loop ResetTheDevice
  where
    loop state = do 
      cmd <- takeMVar $ mvar ih
      case cmd of
        Stop s -> do
          putMVar s ()
          return ()
        _ -> do
          case state of
            ResetTheDevice -> do
              x <- resetTheDevice ih cmd
              loop x



        
