{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

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
  withHandle,
  ) where

--
-- Import section
--
import           System.Hardware.Serialport (CommSpeed (..), Parity (..),
                                             SerialPortSettings (..),
                                             StopBits (..),
                                             defaultSerialSettings, hOpenSerial)
import qualified System.IO                  as SIO

import           Control.Concurrent.Chan    (Chan, newChan, readChan, writeChan)
import           Control.Concurrent.MVar    (MVar, newEmptyMVar, newMVar,
                                             putMVar, takeMVar)
import           Control.Exception
import           Control.Monad
import           Control.Monad.Managed      (Managed, managed, runManaged)
--import Control.Monad.Reader (MonadReader(..), ReaderT(..), ask, runReaderT)

import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Trans.Reader (ReaderT(..), ask, runReaderT)

import           Pipes

import qualified Pipes.Binary               as PB (DecodingError (..),
                                                   decodeGet)
import qualified Pipes.ByteString           as PBS (fromLazy, hGetSome,
                                                    toHandle)
import qualified Pipes.Parse                as PP
import qualified Pipes.Prelude              as P (mapM_, repeatM, mapM)
import           Pipes.Safe

import           Data.ByteString            (ByteString,concat,unpack)
import           Data.Word                  (Word8)
import Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put (Put, putWord8)
--
-- Internal import section
--
import           RFXCom.Message.Base        (Message(..))
import qualified          RFXCom.Message.BaseMessage as BM (RFXComMessage(..))
import qualified          RFXCom.Message.InterfaceControl as IC (Body(..), Command(..))
import           RFXCom.Message.Decoder     (msgParser)
import           RFXCom.System.Concurrent   (forkChild, waitForChildren)
import           RFXCom.System.Exception    (ResourceException (..))
import qualified RFXCom.System.Log as Log

--import qualified RFXCom.System.Log          as Log (Handle (..),
--                                                    _debug, _error,
--                                                   _info, _warning,
--                                                    MonadLogger (..),
--                                                    LoggerT(..), runLoggerT)
       
import RFXCom.Message.Encoder (msgEncoder)

-- |The configuration of the RFXCom writer process settings
data Config = Config


-- |Default RFXCom writer process setting
defaultConfig = Config


-- |The messages to the writer thread
data Message = Message RFXCom.Message.Base.Message -- ^The message to encode and send to the RFXCom device
             | Stop (MVar ())                      -- ^Stop the writer thread
             | Flush                               -- ^Flush the serial port

-- |The service handle to the communcation processes.
newtype Handle = Handle {
  send::RFXCom.Control.RFXComWriter.Message->IO () -- ^The send function that sends a bytestring to theRFXCom device
  }

-- |The internal service handle to the communcation processes.
data IHandle = IHandle {
  loggerH::Log.Handle        -- ^The handle to the logger service
  , serialH::SIO.Handle      -- ^The handle to the serial port
  , mvar::MVar (RFXCom.Control.RFXComWriter.Message)}   -- ^The communcation mvar

--
--
--

newtype WriterThread m a = WriterThread (ReaderT IHandle (Log.LoggerT m) a)
  deriving (Functor, Applicative, Monad, MonadReader IHandle, MonadIO)

--deriving instance (Log.MonadLogger m) => Log.MonadLogger (ReaderT r m)

deriving instance (MonadIO m, Log.MonadLogger (ReaderT IHandle (Log.LoggerT m))) => Log.MonadLogger (WriterThread m)
  
newtype Easy m a = Easy (Log.LoggerT m a)
  deriving (Functor, Applicative, Monad, MonadIO, Log.MonadLogger)

doit::Easy IO ()
doit = do
  Log.info ""
  liftIO $ putStrLn ""
  
    
ddoit::WriterThread IO ()
ddoit = do
  --Log.whop 
--  h <- ask
--  Log.info ""
  liftIO $ putStrLn ""
  return ()

runWriterThread::WriterThread m a -> IHandle -> Log.Handle -> m a
runWriterThread (WriterThread m) c1 c2 = Log.runLoggerT (runReaderT m c1) c2

--
--
--

-- |Performs an IO action with the RFXCom writer process. Note that for each withHandle
-- a new RFXCom writer communication thread will be started.
withHandle::Config->SIO.Handle->Log.Handle->(Handle->IO a)->IO a
withHandle config serialH loggerH io = do
  ih <- IHandle loggerH serialH <$> newEmptyMVar
  tid <- forkChild $ writerThread ih
  x <- io $ Handle { send = sendMessage ih}
  s <- stopWriterThread $ mvar ih
  return x
  where
    sendMessage ih s = do
      putMVar (mvar ih) $ s


--
-- The serial port writer functions
--

stopWriterThread::MVar (RFXCom.Control.RFXComWriter.Message)-> IO ()
stopWriterThread mvar = do
  s<-newEmptyMVar
  putMVar mvar $ Stop s
  takeMVar s
  
-- |The writer threads that writes messages to the RFXCom device. The messages comes via a
-- communcation channel to the writer.
writerThread::IHandle->IO ()
writerThread ih = do
  Log._info (loggerH ih) "RFXCom.Control.RFXComWriter.writeThread: Writer thread is up and running"  
  processSerialPortWriter ih

-- |Waits for a message to arrive on the communcation channel and then injects it into
-- the pipe stream down towards the RFXCom device.
dataProducer::(MonadIO k, MonadMask k)=>IHandle
            ->Word8
            ->Producer ByteString (SafeT k) ()
dataProducer ih seq = do
  cmd <- liftIO $ takeMVar $ mvar ih
  case cmd of
    Message msg@(InterfaceControl IC.Body {IC._cmnd=IC.Reset}) -> do
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
processSerialPortWriter :: (MonadIO m, MonadMask m) =>
                           IHandle
                        -> m () -- ^The result of the pipe execution session
processSerialPortWriter ih = runEffect . runSafeP $ do  
  dataProducer ih 1
  >-> PBS.toHandle (serialH ih)
