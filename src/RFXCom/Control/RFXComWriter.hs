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
import           Control.Monad.State             (MonadState (..),
                                                  StateT (..), get, put, evalStateT)
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

-- |Performs an IO action with the RFXCom Writer. Note that for each withHandle
-- a new RFXCom Writer thread will be started. There should only be one within
-- the application (TOFIX)
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

-- |The RFXCom Writer bootstrap thread that initializes and runs the RFXCOM Writer.
writerThread::Environment
            ->IO ()
writerThread ih = do
  Log._info (loggerH ih) "RFXCom.Control.RFXComWriter.writeThread: Writer thread is up and running"
  runRFXComWriter processSerialPortWriter ih 1

--
-- RFXCom Writer Monad
--

-- |The monad that the RFXCom Writer executes under
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

-- |Waits for a message to arrive on the communcation channel and then injects it into
-- the pipe stream down towards the RFXCom device.
serialMessageSender::(MonadIO k, MonadMask k)=>RFXComWriter (Producer ByteString (SafeT k)) ()
serialMessageSender = do
  env <- ask
  cmd <- liftIO . takeMVar $ mvar env
  case cmd of

    --
    -- This is the message that resets the RFXCom Device, we also need to reset
    -- the message sequence number counter
    --
    Message msg@(B.InterfaceControl IC.Body {IC._cmnd=IC.Reset}) -> do
      Log.info $ "RFXCom.Control.RFXComWriter.dataProducer: Sending " ++ show msg
      bs <- return $ msgEncoder 0 msg -- A bit ugly ;-)
      Log.info $ "Sending : " ++ (show (unpack bs))
      lift . yield $ bs
      put 1 -- We need to reset the message counter
      serialMessageSender

    --
    -- This is the message that will be send to the RFXCom Device and increase
    -- the message sequence number counter
    --
    Message msg -> do
      Log.info $ "RFXCom.Control.RFXComWriter.dataProducer: Sending " ++ show msg
      seq <- get
      bs <- return $ msgEncoder seq msg
      Log.info $ "RFXCom.Control.RFXComWriter.dataProducer: Sending " ++ (show (unpack bs))
      lift . yield $ bs
      put (seq+1) -- Take the next message number in the sequence
      serialMessageSender

    --
    -- This is the message that flushes the serial port to the RFXCom Device
    --
    Flush -> do
      Log.info $ "RFXCom.Control.RFXComWriter.dataProducer: Flushing the serial device"
      liftIO $ SIO.hFlush $ serialH env
      serialMessageSender -- We have not used the current message number so keep it

    --
    -- This is the message that stops the RFXCom Writer
    --
    Stop s -> do
      liftIO $ putMVar s ()
      return ()
      

-- |Run the pipe stream for writing messages to the RFXCom device.
processSerialPortWriter :: (Monad m, MonadIO m, MonadMask m) => RFXComWriter m () -- ^The result of the pipe execution session
processSerialPortWriter = do
  env <- ask
  runEffect . runSafeP $ do
    runRFXComWriter serialMessageSender env 1
    >-> PBS.toHandle (serialH env)
