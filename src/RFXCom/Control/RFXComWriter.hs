{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- Not needed anymore!(?)
--
--{-# LANGUAGE StandaloneDeriving #-}
--{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE UndecidableInstances #-}

-- |This module holds the control process for writing messages to the RFXCom device
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.Control.RFXComWriter (
  Handle(..),
  Config(..),
  RFXCom.Control.RFXComWriter.Message(..),
  defaultConfig,
  withHandle
  ) where

--
-- Import section
--
import qualified System.IO                       as SIO

import           Control.Concurrent.MVar         (MVar, newEmptyMVar, newMVar,
                                                  putMVar, takeMVar)
import           Control.Monad.Reader            (MonadReader (..),
                                                  ReaderT (..), ask, runReaderT)
import           Control.Monad.Catch             (MonadMask, MonadCatch, MonadThrow)

import           Pipes
import qualified Pipes.Binary                    as PB (DecodingError (..),
                                                        decodeGet)
import qualified Pipes.ByteString                as PBS (fromLazy, hGetSome,
                                                         toHandle)
import qualified Pipes.Parse                     as PP
import qualified Pipes.Prelude                   as P (mapM, mapM_, repeatM)
import           Pipes.Safe


import           Data.Binary.Put                 (runPut)
import           Data.Binary.Put                 (Put, putWord8)
import           Data.ByteString                 (ByteString, concat, unpack)
import qualified Data.ByteString.Lazy            as BL
import           Data.Word                       (Word8)

--
-- Internal import section
--
import qualified RFXCom.Message.Base             as B (Message (..))
import qualified RFXCom.Message.BaseMessage      as BM (RFXComMessage (..))
import           RFXCom.Message.Decoder          (msgParser)
import qualified RFXCom.Message.InterfaceControl as IC (Body (..), Command (..))
import           RFXCom.System.Concurrent        (forkChild)
import qualified RFXCom.System.Log               as Log

import           RFXCom.Message.Encoder          (msgEncoder)

-- |The configuration of the RFXCom writer process settings
data Config = Config


-- |The default RFXCom Writer settings
defaultConfig = Config


-- |The messages that can be sent to the RFXCom Writer to be able to control it
data Message = Message B.Message -- ^Encode and send the message to the RFXCom Device
             | Stop (MVar ())    -- ^Stop the RFXCom Writer
             | Flush             -- ^Flush the serial port to the RFXCom Device

-- |The handle to the RFXCom Writer. It is used by all the clients of the RFXCom Writer
newtype Handle = Handle {
  send::Message->IO () -- ^The send function that sends a command message to the RFXCom Writer
  }

-- |The environment for the RFXCom Writer
data Environment = Environment {
  loggerH  :: Log.Handle -- ^The handle to the logger
  ,serialH :: SIO.Handle -- ^The handle to the serial port
  ,mvar    :: MVar (Message) -- ^The communication port to the RFXCom Writer
  }

--
-- RFXCom Writer
--

-- |The monad that the RFXCom Writer executes under
newtype RFXComWriter m a = RFXComWriter (ReaderT Environment (Log.LoggerT m) a)
  deriving (Functor, Applicative, Monad, MonadReader Environment, MonadIO, MonadMask, MonadCatch, MonadThrow, Log.MonadLogger)

type RFXComWriterIO a = RFXComWriter IO a

-- |Injects the environment and runs the computations in the RFXCom Writer monad
runRFXComWriter::RFXComWriter m a -> Environment -> m a
runRFXComWriter (RFXComWriter m) env = Log.runLoggerT (runReaderT m env) (loggerH env)

--
-- Scratchpad area, to be removed
--
-- ddoit::RFXComWriter IO ()
-- ddoit = do
--   h <- ask
--   Log.info ""
--   liftIO $ putStrLn ""
--   return ()
--

-- |Performs an IO action with the RFXCom writer process. Note that for each withHandle
-- a new RFXCom writer communication thread will be started.
withHandle::Config->SIO.Handle->Log.Handle->(Handle->IO a)->IO a
withHandle config serialH loggerH io = do
  env <- Environment loggerH serialH <$> newEmptyMVar
  tid <- forkChild $ writerThread env
  x <- io $ Handle { send = sendMessage env}
  s <- stopWriterThread $ mvar env
  return x
  where
    sendMessage ih s = do
      putMVar (mvar ih) $ s


--
-- The serial port writer functions
--

-- |Stops the RFXCom Writer by sending the stop message
stopWriterThread::MVar Message -- ^The destination for the message
                -> IO ()
stopWriterThread mvar = do
  s<-newEmptyMVar
  putMVar mvar $ Stop s
  takeMVar s

-- |The writer threads that writes messages to the RFXCom device. The messages comes via a
-- communcation channel to the writer.
writerThread::Environment
            ->IO ()
writerThread ih = do
  Log._info (loggerH ih) "RFXCom.Control.RFXComWriter.writeThread: Writer thread is up and running"
  runRFXComWriter processSerialPortWriter ih

-- |Waits for a message to arrive on the communcation channel and then injects it into
-- the pipe stream down towards the RFXCom device.
dataProducer::(MonadIO k, MonadMask k)=>Environment
            ->Word8
            ->Producer ByteString (SafeT k) ()
dataProducer ih seq = do
  cmd <- liftIO $ takeMVar $ mvar ih
  case cmd of
    Message msg@(B.InterfaceControl IC.Body {IC._cmnd=IC.Reset}) -> do
      liftIO $ Log._info (loggerH ih) $ "RFXCom.Control.RFXComWriter.dataProducer: Sending " ++ show msg
      bs <- return $ msgEncoder 0 msg
      liftIO $ Log._info (loggerH ih) $ "Sending : " ++ (show (unpack bs))
      yield $ bs
      dataProducer ih 1
    Message msg -> do
      liftIO $ Log._info (loggerH ih) $ "RFXCom.Control.RFXComWriter.dataProducer: Sending " ++ show msg
      bs <- return $ msgEncoder seq msg
      liftIO $ Log._info (loggerH ih) $ "Sending : " ++ (show (unpack bs))
      yield $ bs
      dataProducer ih (seq+1)
    Flush -> do
      liftIO $ Log._info (loggerH ih) $ "RFXCom.Control.RFXComWriter.dataProducer: Flushing the serial device"
      liftIO $ SIO.hFlush $ serialH ih
      dataProducer ih seq
    Stop s -> do
      liftIO $ putMVar s ()
      return ()

-- |Run the pipe stream for writing messages to the RFXCom device.
processSerialPortWriter :: (Monad m, MonadIO m, MonadMask m) => RFXComWriter m () -- ^The result of the pipe execution session
processSerialPortWriter = do
  env <- ask
  runEffect . runSafeP $ do
    dataProducer env 1
    >-> PBS.toHandle (serialH env)


