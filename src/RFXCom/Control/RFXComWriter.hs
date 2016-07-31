-- |This module holds the control process for writing messages to the RFXCom device
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.Control.RFXComWriter (
  Handle(..),
  Config(..),
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
import qualified RFXCom.System.Log          as Log (Handle (..), debug, error,
                                                    info, warning)

-- |The configuration of the RFXCom writer process settings
data Config = Config


-- |Default RFXCom writer process setting
defaultConfig = Config


-- |The messages to the writer thread
data Message = Message ByteString
             | Stop (MVar ())


-- |The service handle to the communcation processes.
newtype Handle = Handle {
  send::ByteString->IO () -- ^The send function that sends a bytestring to theRFXCom device
  }


-- |The internal service handle to the communcation processes.
data IHandle = IHandle {
  loggerH::Log.Handle        -- ^The handle to the logger service
  , serialH::SIO.Handle      -- ^The handle to the serial port
  , mvar::MVar RFXCom.Control.RFXComWriter.Message}   -- ^The communcation mvar


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
      putMVar (mvar ih) $ Message s


--
-- The serial port writer functions
--

stopWriterThread::MVar RFXCom.Control.RFXComWriter.Message -> IO ()
stopWriterThread mvar = do
  s<-newEmptyMVar
  putMVar mvar $ Stop s
  takeMVar s
  
-- |The writer threads that writes messages to the RFXCom device. The messages comes via a
-- communcation channel to the writer.
writerThread::IHandle->IO ()
writerThread ih = do
  Log.info (loggerH ih) "RFXCom.Control.RFXComWriter.writeThread: Writer thread is up and running"  
  processSerialPortWriter ih


-- |Waits for a message to arrive on the communcation channel and then injects it into
-- the pipe stream down towards the RFXCom device.
dataProducer::(MonadIO k, MonadMask k)=>MVar RFXCom.Control.RFXComWriter.Message -> Producer ByteString (SafeT k) ()
dataProducer m = do
  cmd <- liftIO $ takeMVar m
  case cmd of
    Message bs -> do
      yield bs
      dataProducer m
    Stop s -> do
      liftIO $ putMVar s ()
      return ()

-- |Run the pipe stream for writing messages to the RFXCom device.
processSerialPortWriter :: (MonadIO m, MonadMask m) =>
                           IHandle
                        -> m () -- ^The result of the pipe execution session
processSerialPortWriter ih = runEffect . runSafeP $ do  
  dataProducer $ mvar ih
  >-> PBS.toHandle (serialH ih)