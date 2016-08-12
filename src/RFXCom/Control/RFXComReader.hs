{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |This module holds the control processes for reading the message from the RFXCom
-- device and sending them off to the master controller
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.Control.RFXComReader (
  Handle(..),
  Config(..),
  defaultConfig,
  withHandle
  ) where

--
-- Import section
--
import           System.Hardware.Serialport  (CommSpeed (..), Parity (..),
                                              SerialPortSettings (..),
                                              StopBits (..),
                                              defaultSerialSettings,
                                              hOpenSerial)
import qualified System.IO                   as SIO

import           Control.Concurrent          (killThread)
import           Control.Concurrent.Chan     (Chan, newChan, readChan,
                                              writeChan)
import           Control.Concurrent.MVar     (MVar, newEmptyMVar, newMVar,
                                              putMVar, takeMVar)
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Managed       (Managed, managed, runManaged)
import           Control.Monad.Reader        (MonadReader (..), ReaderT (..),
                                              ask, runReaderT)

import           Pipes

import qualified Pipes.Binary                as PB (DecodingError (..),
                                                    decodeGet)
import qualified Pipes.ByteString            as PBS (fromLazy, hGetSome,
                                                     toHandle)
import qualified Pipes.Parse                 as PP
import qualified Pipes.Prelude               as P (mapM, mapM_, repeatM)
import           Pipes.Safe

import           Data.ByteString             (ByteString)
import           Data.Word                   (Word8)

--
-- Internal import section
--
import qualified RFXCom.Control.RFXComMaster as RFXComM (Handle (..),
                                                         Message (..))
import           RFXCom.Message.Base         (Message)
import           RFXCom.Message.Decoder      (msgParser)
import           RFXCom.System.Concurrent    (forkChild, waitForChildren)
import           RFXCom.System.Exception     (ResourceException (..))
import qualified RFXCom.System.Log           as Log (Handle (..), LoggerT (..),
                                                     MonadLogger (..), _debug,
                                                     _error, _info, _warning,
                                                     runLoggerT)

-- |The configuration of the RFXCom Serial device reader processes
data Config = Config


-- |Default RFXCom reader settings
defaultConfig = Config


-- |The service handle to the communcation processes.
data Handle = Handle


-- |The RFXCom Reader environment that it executes with
data Environment =  Environment {
  loggerH   :: Log.Handle      -- ^The handle to the logger service
  , serialH :: SIO.Handle      -- ^The handle to the serial port
  , masterH :: RFXComM.Handle  -- ^The handle to the master process
  }


-- |Performs an IO action with the RFXCom reader process. Note that for each withHandle
-- a new RFXCom reader thread will be started.
withHandle::Config          -- ^The configuration of the Reader thread
          ->SIO.Handle      -- ^The serial port handle
          ->Log.Handle      -- ^The Logger service handle
          ->RFXComM.Handle  -- ^The Master service handle
          ->(Handle->IO a)  -- ^The IO action
          ->IO a
withHandle config serialH loggerH masterH io = do
  tid <- forkChild $ readerThread $ Environment loggerH serialH masterH
  x <- io $ Handle
  killThread tid -- Not nice, but I did not come to think of any easier way for now (TOFIX)
  return x


--
-- RFXCom Reader Monad
--

-- |The monad that the RFXCom Reader executes under
newtype RFXComReader m a = RFXComReader (ReaderT Environment (Log.LoggerT m) a)
  deriving (Functor, Applicative, Monad, MonadReader Environment, MonadIO,
            MonadMask, MonadCatch, MonadThrow, Log.MonadLogger)


-- |The lift for the RFXCom Reader monad
instance MonadTrans RFXComReader where
  lift m = RFXComReader $ lift $ lift m


-- |Injects the environment and runs the computations in the RFXCom Reader monad
runRFXComReader::(Monad m) => RFXComReader m a -- ^The RFXCom Reader monad
               -> Environment     -- ^The environment that the monad should be evaluated under
               ->m a              -- ^The result
runRFXComReader (RFXComReader m) env = Log.runLoggerT (runReaderT m env) (loggerH env)

--
-- The serial port reader functions
--

kkk::Environment->Message->IO()
kkk ih msg = do
  Log._info (loggerH ih) $ "RFXCom.Control.RFXComReader.readThread: Read " ++ show msg
  (RFXComM.send $ masterH ih) $ RFXComM.Message msg

koko::Environment->MaybeMessage->IO ()
koko ih msg = either (return . const ()) (kkk ih) msg

-- |The reader thread that reads all messages from the RFXCom device and sends them away to
-- some handler.
readerThread::Environment
            ->IO ()
readerThread env = do
  Log._info (loggerH env) "RFXCom.Control.RFXComReader.readerThread: Reader thread is up and running"
  runRFXComReader (processSerialPort env (koko env)) env

serialMessageReader::(Monad m, MonadIO m, MonadMask m)=>RFXComReader (Producer ByteString (SafeT m) ) ()
serialMessageReader = do
  env <- ask
  lift $ forever $ PBS.hGetSome 1 $ serialH env

serialMessageParser::(Monad m, MonadIO m, MonadMask m)=>RFXComReader (Pipe ByteString MaybeMessage (SafeT m)) ()
serialMessageParser = do
  lift $ PP.parseForever msgParser

serialMessageHandler::(Monad m, MonadIO m, MonadMask m)=>MessageHandler m
                    ->RFXComReader (Consumer MaybeMessage (SafeT m)) ()
serialMessageHandler handler = do
  lift $ P.mapM_ (lift . handler)

type MaybeMessage = Either PB.DecodingError Message
type MessageHandler m = MaybeMessage->m ()

-- |Run the pipe from RFXCom to us
processSerialPort ::
     (Monad m, MonadIO m, MonadMask m)
  => Environment
  -> MessageHandler m -- ^The message handler function
  -> RFXComReader m () -- ^The result of the pipe execution session
processSerialPort ih handler = do
  env <- ask
  lift $ runEffect . runSafeP $ do

    runRFXComReader (serialMessageReader) env
    >->
    runRFXComReader (serialMessageParser) env
    >->
    runRFXComReader (serialMessageHandler handler) env
    
--    forever $ PBS.hGetSome 1 $ serialH ih
--    >-> PP.parseForever msgParser
--    >-> P.mapM_ (lift . handler)
