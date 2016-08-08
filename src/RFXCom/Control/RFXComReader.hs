-- |This module holds the control processes for reading the message from the RFXCom
-- device.
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.Control.RFXComReader (
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

import           Control.Concurrent (killThread)
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
import qualified RFXCom.System.Log          as Log (Handle (..), _debug, _error,
                                                    _info, _warning)
import qualified RFXCom.Control.RFXComMaster as RFXComM (Handle(..),Message(..))

-- |The configuration of the RFXCom Serial device reader processes
data Config = Config


-- |Default RFXCom reader settings
defaultConfig = Config


-- |The service handle to the communcation processes.
data Handle = Handle


-- |The internal service handle to the communication processes.
data IHandle = IHandle {
  loggerH::Log.Handle        -- ^The handle to the logger service
  , serialH::SIO.Handle      -- ^The handle to the serial port
  , masterH::RFXComM.Handle  -- ^The handle to the master process
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
  tid <- forkChild $ readerThread $ IHandle loggerH serialH masterH
  x <- io $ Handle
  killThread tid
  return x


--
-- The serial port reader functions
--

kkk ih msg = do
  Log._info (loggerH ih) $ "RFXCom.Control.RFXComReader.readThread: Read " ++ show msg
  (RFXComM.send $ masterH ih) $ RFXComM.Message msg

koko::IHandle->Either PB.DecodingError Message->IO ()
koko ih msg = either (return . const ()) (kkk ih) msg 

-- |The reader thread that reads all messages from the RFXCom device and sends them away to
-- some handler.
readerThread::IHandle->IO ()
readerThread ih = do
  Log._info (loggerH ih) "RFXCom.Control.RFXComReader.readerThread: Reader thread is up and running"
  processSerialPort ih (koko ih)


terminator :: (MonadIO m, MonadMask m) => Consumer (Either PB.DecodingError Message) (SafeT m) ()
terminator = do
  str <- await
  liftIO $ putStrLn $ show str
  terminator


take ::  (Monad m) => Int -> Pipe a a (SafeT m) ()
take n = do
    replicateM_ n $ do                     -- Repeat this block 'n' times
        x <- await                         -- 'await' a value of type 'a'
        yield x                            -- 'yield' a value of type 'a'


-- |Run the pipe from RFXCom to us
processSerialPort ::
     (MonadIO m, MonadMask m)
  => IHandle->((Either PB.DecodingError Message) -> m ()) -- ^The message handler function
  -> m () -- ^The result of the pipe execution session
processSerialPort ih handler =
  runEffect . runSafeP $ do

    forever $ PBS.hGetSome 1 $ serialH ih
    >-> PP.parseForever msgParser
--    >-> RFXCom.Control.RFXComReader.take 10000
    >-> P.mapM_ (lift . handler)
--    >-> terminator
