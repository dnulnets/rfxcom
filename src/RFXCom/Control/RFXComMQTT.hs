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
module RFXCom.Control.RFXComMQTT (
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
import qualified Network.MQTT                     as MQTT

import           Data.ByteString                  (ByteString)

--
-- Internal import section
--
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

-- |The configuration of the RFXCom MQTT process
data Config = Config {
  host      :: String  -- ^The hostname of the MQTT broker
  ,username :: String  -- ^The username to use when logging into the MQTT broker
  ,password :: String  -- ^The password to use when logging into the MQTT broker
  }

-- |Default RFXCom MQTT configuration
defaultConfig = Config "localhost" "rfxcom" "rfxcom"


-- |The service handle to the MQTT Broker
data Handle = Handle {
  publish         :: String      -- ^The topic
                  ->ByteString   -- ^The message
                  ->IO ()
  ,subscribe      :: String      -- ^The topic
                  ->IO ()
  ,waitForPublish :: STM (MQTT.Message MQTT.PUBLISH)
  }

-- |The internal service handle to the communcation processes.
data Environment = Environment {
  mqtt     :: MQTT.Config
  ,loggerH :: Log.Handle }

-- |Performs an IO action with the RFXCom writer process. Note that for each withHandle
-- a new RFXCom writer communication thread will be started.
withHandle::Config          -- ^The configuration of the master threads
          ->Log.Handle      -- ^The handle to the Logger service
          ->(Handle->IO a)  -- ^The IO action to perform
          ->IO a
withHandle config loggerH io = do
  cmds <- MQTT.mkCommands
  pubChan <- newTChanIO
  let conf = (MQTT.defaultConfig cmds pubChan)
              { MQTT.cUsername = Just $ pack $ username config
              , MQTT.cPassword = Just $ pack $ password config
              , MQTT.cHost = host config
              }
  let env = Environment conf loggerH
  tid <- forkChild $ subscriberThread env
  x <- io $ Handle (_publish conf) (_subscribe conf) (_waitForPublish conf)
  killThread tid
  return x


--
-- RFXCom Master Monad
--

-- |The monad that the RFXCom Master executes under
newtype RFXComSubscriber m a = RFXComSubscriber (ReaderT Environment (Log.LoggerT m) a)
  deriving (Functor, Applicative, Monad, MonadReader Environment, MonadIO,
            Log.MonadLogger)


-- |The lift for the RFXCom Master monad
instance MonadTrans RFXComSubscriber where
  lift m = RFXComSubscriber $ lift $ lift m


-- |Injects the environment and runs the computations in the RFXCom Master monad
runRFXComSubscriber::(Monad m) => RFXComSubscriber m a -- ^The RFXCom Subscriber monad
                   -> Environment     -- ^The environment that the monad should be evaluated under
                   ->m a              -- ^The result
runRFXComSubscriber (RFXComSubscriber m) env = Log.runLoggerT (runReaderT m env) (loggerH env)


-- |The thread that starts up the MQTT subscriber monad and then executes it
subscriberThread::Environment -- ^The environment to execute under
                ->IO ()
subscriberThread env = do
  Log.infoH (loggerH env) "RFXCom.Control.RFXComSubscriber.subscriberThread: Subscriber thread is up and running"
  runRFXComSubscriber processMQTTSubscription env


-- |Subscriberhandler thread
handleMsg::MQTT.Message MQTT.PUBLISH
         ->IO ()
handleMsg msg = do
  putStr "Payload:"
  print $ MQTT.payload $ MQTT.body msg
  return ()

_publish::MQTT.Config->String->ByteString->IO ()
_publish config topic raw = do
    MQTT.publish config MQTT.NoConfirm False (MQTT.toTopic (MQTT.MqttText (pack topic))) raw

_subscribe::MQTT.Config->String->IO ()
_subscribe config topic = do
  _ <- MQTT.subscribe config [((MQTT.toTopic (MQTT.MqttText (pack topic))), MQTT.Handshake)]
  return ()

_waitForPublish::MQTT.Config->STM (MQTT.Message MQTT.PUBLISH)
_waitForPublish config = do
  readTChan $ MQTT.cPublished $ config

-- |The MQTT Subscriber that runs in the RFXComSubscriber monad.
processMQTTSubscription::(Monad m, MonadIO m)=>RFXComSubscriber m ()
processMQTTSubscription = do
  env <- ask
  Log.info "MQTT Started"


  --
  -- The subscription thread, waits for all incoming messages on the rfxcom tree on the MQTT broker
  --
  _ <- liftIO . forkIO $ do
    MQTT.publish (mqtt env) MQTT.NoConfirm False ("rfxcom"::MQTT.Topic) "Jabadabbadata"
    MQTT.subscribe (mqtt env) [("rfxcom/#"::MQTT.Topic, MQTT.Handshake)]
    forever $ atomically (readTChan $ MQTT.cPublished $ mqtt env) >>= handleMsg


  terminated <- liftIO $ MQTT.run $ mqtt env
  Log.info $ "MQTT terminated " ++ show terminated
  return ()
