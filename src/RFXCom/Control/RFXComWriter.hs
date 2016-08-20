{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |This module holds the control process for writing messages to the RFXCom device
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.Control.RFXComWriter (
  Handle(..),
  Config(..),
  Message(..),
  defaultConfig,
  withHandle
  ) where

--
-- External import section
--
import qualified System.IO                       as SIO

import           Control.Concurrent.MVar         (MVar, newEmptyMVar,
                                                  putMVar, takeMVar)

import           Control.Monad.Reader            (MonadReader (..),
                                                  ReaderT (..), ask, runReaderT)
import           Control.Monad.State             (MonadState (..), StateT (..),
                                                  evalStateT, get, put)

import           Pipes
import qualified Pipes.ByteString                as PBS (toHandle)
import           Pipes.Safe

import           Data.Binary.Put                 (Put, putWord8, runPut)
import           Data.ByteString                 (ByteString, unpack)
import           Data.Word                       (Word8)

--
-- Internal import section
--
import qualified RFXCom.Message.Base             as B (Message (..))
import qualified RFXCom.Message.BaseMessage      as BM (RFXComMessage (..))
import           RFXCom.Message.Encoder          (msgEncoder)
import qualified RFXCom.Message.InterfaceControl as IC (Body (..), Command (..))

import           RFXCom.System.Concurrent        (forkChild)
import qualified RFXCom.System.Log               as Log

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

-- |Performs an IO action with the RFXCom Writer. Note that for each withHandle
-- a new RFXCom Writer thread will be started. There should only be one within
-- the application (TOFIX)
withHandle::Config
          ->SIO.Handle
          ->Log.Handle
          ->(Handle->IO a)
          ->IO a
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
-- RFXCom Writer Monad
--

-- |The monad that the RFXCom Writer executes under. The reader holds the environment and
-- the state hoolds the current message counter.
newtype RFXComWriter m a = RFXComWriter (ReaderT Environment (StateT Word8 (Log.LoggerT m)) a)
  deriving (Functor, Applicative, Monad, MonadReader Environment, MonadIO,
            MonadMask, MonadCatch, MonadThrow, MonadState Word8, Log.MonadLogger)


-- |The lift for the RFXCom Writer monad
instance MonadTrans RFXComWriter where
  lift m = RFXComWriter $ lift $ lift $ lift m


-- |Injects the environment and runs the computations in the RFXCom Writer monad
runRFXComWriter::(Monad m) => RFXComWriter m a -- ^The RFXCom Writer monad
               -> Environment     -- ^The environment that the monad should be evaluated under
               -> Word8           -- ^The initial state of the RFXCom Writer monad
               ->m a              -- ^The result
runRFXComWriter (RFXComWriter m) env state = Log.runLoggerT (evalStateT (runReaderT m env) state) (loggerH env)


--
-- Thread handling
--

-- |Stops the RFXCom Writer by sending the stop message
stopWriterThread::MVar Message -- ^The destination for the message
                -> IO ()
stopWriterThread mvar = do
  s<-newEmptyMVar
  putMVar mvar $ Stop s
  takeMVar s

-- |The RFXCom Writer bootstrap thread that initializes and runs the RFXCOM Writer.
writerThread::Environment
            ->IO ()
writerThread env = do
  Log.infoH (loggerH env) "RFXCom.Control.RFXComWriter.writeThread: Writer thread is up and running"
  runRFXComWriter processSerialPortWriter env 1

--
-- Pipe handling
--

-- |Waits for a message to arrive on the communcation channel, encode it to bytes, add message number  and injects
-- it downstreams into the pipe.
serialMessageProducer::(Monad m, MonadIO m, MonadMask m)=>RFXComWriter (Producer ByteString (SafeT m)) ()
serialMessageProducer = do
  env <- ask
  cmd <- liftIO . takeMVar $ (mvar env)
  case cmd of

    --
    -- This is the message that resets the RFXCom Device, we also need to reset
    -- the message sequence number counter
    --
    Message msg@(B.InterfaceControl IC.Body {IC._cmnd=IC.Reset}) -> do
      Log.info $ "RFXCom.Control.RFXComWriter.dataProducer: Sending " ++ show msg
      bs <- return $ msgEncoder 0 msg -- A bit ugly ;-)
      Log.info $ "Sending : " ++ (show (unpack bs))
      lift $ yield $ bs
      put 1 -- We need to reset the message counter
      serialMessageProducer

    --
    -- This is any ohter RFXCom device message that will be sent to the RFXCom Device and increase
    -- the message sequence number counter
    --
    Message msg -> do
      Log.info $ "RFXCom.Control.RFXComWriter.dataProducer: Sending " ++ show msg
      seq <- get
      bs <- return $ msgEncoder seq msg
      Log.info $ "RFXCom.Control.RFXComWriter.dataProducer: Sending " ++ (show (unpack bs))
      lift . yield $ bs
      put (seq+1) -- Take the next message number in the sequence
      serialMessageProducer

    --
    -- This is the message that flushes the serial port to the RFXCom Device
    --
    Flush -> do
      Log.info $ "RFXCom.Control.RFXComWriter.dataProducer: Flushing the serial device"
      liftIO $ SIO.hFlush $ serialH env
      serialMessageProducer -- We have not used the current message number so keep it

    --
    -- This is the message that stops the RFXCom Writer
    --
    Stop s -> do
      liftIO $ putMVar s ()
      return ()

-- |Consumes bytes and sends them to the serial port.
serialMessageSender::(Monad m, MonadIO m, MonadMask m)=>RFXComWriter (Consumer ByteString (SafeT m)) ()
serialMessageSender = do
  env <- ask
  lift $ PBS.toHandle (serialH env)


-- |Run the pipe stream for writing messages to the RFXCom device.
processSerialPortWriter :: (Monad m, MonadIO m, MonadMask m) => RFXComWriter m () -- ^The result of the pipe execution session
processSerialPortWriter = do
  env <- ask
  runEffect . runSafeP $ do
    runRFXComWriter (serialMessageProducer) env 1
    >->
    PBS.toHandle (serialH env)
