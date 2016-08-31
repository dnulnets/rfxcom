{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- |This module holds the MQTT Subscriber control process for the RFXCom device
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.Control.RFXComPublisher (
  Handle(..),
  Config(..),
  defaultConfig,
  withHandle,
  ) where

--
-- Import section
--
import           Control.Concurrent               (ThreadId, forkIO, killThread,
                                                   myThreadId)
import           System.Exit                      (exitFailure)
import qualified System.IO                        as SIO


import           Control.Concurrent.Chan          (Chan, newChan, readChan,
                                                   writeChan)
import           Control.Concurrent.MVar          (MVar, newEmptyMVar, putMVar,
                                                   takeMVar)


import           Control.Concurrent.STM           (atomically, newTChanIO,
                                                   readTChan)

import           Control.Monad
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.Reader             (MonadReader (..),
                                                   ReaderT (..), ask,
                                                   runReaderT)
import           Control.Monad.State              (MonadState (..), StateT (..),
                                                   evalStateT, get, put)
import           Control.Monad.STM                (STM (..))
import           Control.Monad.Trans.Class        (MonadTrans (..))

import           Data.Text                        (pack)

import           Data.ByteString                  (ByteString)

--
-- Internal import section
--
import qualified RFXCom.Control.RFXComMaster      as RFXComM (Handle (..),
                                                              Message (..))

import qualified RFXCom.Message.Base              as MB
import qualified RFXCom.Message.InterfaceControl  as IC (Body (..),
                                                         Command (..))
import qualified RFXCom.Message.InterfaceResponse as IR (Body (..))

import           RFXCom.System.Concurrent         (executeAfterDelay, forkChild)
import           RFXCom.System.Exception          (ResourceException (..))

import qualified RFXCom.System.Log                as Log (Handle (..), LoggerT,
                                                          MonadLogger (..),
                                                          debugH, errorH, infoH,
                                                          runLoggerT, warningH)

import qualified RFXCom.Control.RFXComMQTT as MQTT (Handle(..), publish)

-- |The configuration of the RFXCom MQTT publisher
data Config = Config {
  base      :: String  -- ^The base topic for all published messages
  }


-- |Default RFXCom MQTT Publisher configuration
defaultConfig = Config "rfxcom"

-- |The messages that can be sent to the RFXCom Writer to be able to control it
data Message = Message MB.Message -- ^Encode and send the message to the RFXCom Device
             | Stop (MVar ())    -- ^Stop the RFXCom Writer


-- |The service handle to the MQTT Publisher
data Handle = Handle {
  
  -- |Published as message to the MQTT Broker
  send :: Message -- ^The message that needs to be published
       ->IO ()
  }


-- |The internal service handle to the communcation processes.
data Environment = Environment {
  mqtt     :: MQTT.Handle
  ,loggerH :: Log.Handle
  ,mvar     :: MVar Message }

-- |Performs an IO action with the RFXCom writer process. Note that for each withHandle
-- a new RFXCom writer communication thread will be started.
withHandle::Config          -- ^The configuration of the master threads
          ->Log.Handle      -- ^The handle to the Logger service
          ->MQTT.Handle     -- ^The handle to the MQTT Broker
          ->(Handle->IO a)  -- ^The IO action to perform
          ->IO a
withHandle config loggerH mqttH io = do
  env <- Environment mqttH loggerH <$> newEmptyMVar 
  tid <- forkChild $ publisherThread env
  x <- io $ Handle $ sendMessage env
  stopPublisherThread env
  return x
  where
    
    sendMessage env msg = do
      putMVar (mvar env) msg

    stopPublisherThread env = do
      s<-newEmptyMVar
      putMVar (mvar env) $ Stop s
      takeMVar s


--
-- RFXCom Publisher Monad
--

-- |The monad that the RFXCom Master executes under
newtype RFXComPublisher m a = RFXComPublisher (ReaderT Environment (Log.LoggerT m) a)
  deriving (Functor, Applicative, Monad, MonadReader Environment, MonadIO,
            Log.MonadLogger)


-- |The lift for the RFXCom Master monad
instance MonadTrans RFXComPublisher where
  lift m = RFXComPublisher $ lift $ lift m


-- |Injects the environment and runs the computations in the RFXCom Publisher monad
runRFXComPublisher::(Monad m) => RFXComPublisher m a -- ^The RFXCom Publisher monad
                   -> Environment     -- ^The environment that the monad should be evaluated under
                   ->m a              -- ^The result
runRFXComPublisher (RFXComPublisher m) env = Log.runLoggerT (runReaderT m env) (loggerH env)

  
-- |The thread that starts up the MQTT publisher monad and then executes it
publisherThread::Environment -- ^The environment to execute under
                ->IO ()
publisherThread env = do
  Log.infoH (loggerH env) "RFXCom.Control.RFXComPublisher.publisherThread: Publisher thread is up and running"
  runRFXComPublisher processMQTTPublisher env


-- |The MQTT Publisher that runs in the RFXComPublisher monad.
processMQTTPublisher::(Monad m, MonadIO m)=>RFXComPublisher m ()
processMQTTPublisher = do
  env <- ask
  cmd <- liftIO . takeMVar $ (mvar env)
  case cmd of

    Message msg -> do
      liftIO $ MQTT.publish (mqtt env) "" ""
      processMQTTPublisher

    Stop s -> do
      liftIO $ putMVar s ()
      return ()

