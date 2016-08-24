{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# Language DataKinds, OverloadedStrings #-}

-- |This module holds the MQTT Subscriber control process for the RFXCom device
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.Control.RFXComSubscriber (
  Handle(..),
  Config(..),
  defaultConfig,
  withHandle,
  ) where

--
-- Import section
--
import qualified System.IO                        as SIO
import  System.Exit (exitFailure)
import           Control.Concurrent               (ThreadId, killThread,
                                                   myThreadId, forkIO)
                 
import           Control.Concurrent.MVar          (MVar, newEmptyMVar, putMVar,
                                                   takeMVar)
import Control.Concurrent.STM (newTChanIO, readTChan, atomically)

import           Control.Monad
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.Reader             (MonadReader (..),
                                                   ReaderT (..), ask,
                                                   runReaderT)
import           Control.Monad.State              (MonadState (..), StateT (..),
                                                   evalStateT, get, put)
import           Control.Monad.Trans.Class        (MonadTrans (..))
import qualified Network.MQTT                     as MQTT
import Data.Text (pack)

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

-- |The configuration of the RFXCom master process settings
data Config = Config {
  host      :: String
  ,username :: String
  ,password :: String
  }

-- |Default RFXCom master process setting
defaultConfig = Config "localhost" "rfxcom" "rfxcom" 


-- |The service handle to the communcation processes.
data Handle = Handle

-- |The internal service handle to the communcation processes.
data Environment = Environment {
  mqtt :: MQTT.Config
  ,loggerH :: Log.Handle
  ,masterH :: RFXComM.Handle }

-- |Performs an IO action with the RFXCom writer process. Note that for each withHandle
-- a new RFXCom writer communication thread will be started.
withHandle::Config          -- ^The configuration of the master threads
          ->Log.Handle      -- ^The handle to the Logger service
          ->RFXComM.Handle  -- ^The handle to the RFXCom Writer service
          ->(Handle->IO a)  -- ^The IO action to perform
          ->IO a
withHandle config loggerH masterH io = do
  cmds <- MQTT.mkCommands
  pubChan <- newTChanIO
  let conf = (MQTT.defaultConfig cmds pubChan)
              { MQTT.cUsername = Just $ pack $ username config
              , MQTT.cPassword = Just $ pack $ password config
              , MQTT.cHost = host config
              }
  let env = Environment conf loggerH masterH
  tid <- forkChild $ subscriberThread env
  x <- io $ Handle
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


-- |Sends a message to the master thread.
sendToMaster::MVar RFXComM.Message -- ^The internal handle to the master thread
            ->RFXComM.Message      -- ^Tĥe message to send
            ->IO ()
sendToMaster mvar msg = do
  putMVar mvar msg


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


-- |The MQTT Subscriber that runs in the RFXComSubsciber monad.
processMQTTSubscription::(Monad m, MonadIO m)=>RFXComSubscriber m ()
processMQTTSubscription = do
  env <- ask
  Log.info "MQTT Started"

  _ <- liftIO . forkIO $ do
    qosGranted <- MQTT.subscribe (mqtt env) [("rfxcom"::MQTT.Topic, MQTT.Handshake)]
    case qosGranted of
      [MQTT.Handshake] -> forever $ atomically (readTChan $ MQTT.cPublished $ mqtt env) >>= handleMsg
      _ -> do
        putStrLn $ "Wanted QoS Handshake, got " ++ show qosGranted
        exitFailure


  terminated <- liftIO $ MQTT.run $ mqtt env
  Log.info $ "MQTT terminated " ++ show terminated
  return ()
