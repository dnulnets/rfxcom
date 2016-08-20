{-# OPTIONS_HADDOCK ignore-exports #-}
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

import qualified System.IO                   as SIO

import           Control.Concurrent          (killThread)
import           Control.Concurrent.MVar     (MVar)
import           Control.Monad
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
                                                     MonadLogger (..),
                                                     runLoggerT, debugH, errorH,
                                                     infoH, warningH)

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
            Log.MonadLogger)


-- |The lift for the RFXCom Reader monad
instance MonadTrans RFXComReader where
  lift m = RFXComReader $ lift $ lift m

-- |A type synonym for a message or decoding error
type MaybeMessage = Either PB.DecodingError Message

-- |A type synonym for a message handler
type MessageHandler m = MaybeMessage->m ()

-- |Injects the environment and runs the computations in the RFXCom Reader monad
runRFXComReader::(Monad m) => RFXComReader m a -- ^The RFXCom Reader monad
               -> Environment     -- ^The environment that the monad should be evaluated under
               ->m a              -- ^The result
runRFXComReader (RFXComReader m) env = Log.runLoggerT (runReaderT m env) (loggerH env)


-- |The reader thread that reads all messages from the RFXCom device and sends them away to
-- the RFXCom Master handler by using the 'maybeSendMessage' function.
readerThread::Environment
            ->IO ()
readerThread env = do
  Log.infoH (loggerH env) "RFXCom.Control.RFXComReader.readerThread: Reader thread is up and running"
  runRFXComReader (processSerialPort (maybeSendMessage (masterH env))) env

-- |Sends an RFXCom device message to the RFXCom Master for further handling
maybeSendMessage::(Monad m, MonadIO m, MonadMask m)=>RFXComM.Handle -- ^The RFXCom Master handle
                ->MaybeMessage                                      -- ^The RFXCom device message
                ->m ()
maybeSendMessage h msg = do
  either (return . const ()) (sendMessage h) msg
  where
    sendMessage h msg = do
      liftIO $ RFXComM.send h $ RFXComM.Message msg

-- |The modelled effect of the producer, consumer and pipe for reading and transforming the serial
-- byte stream to a stream of RFXCom device messages and then sending them off to a message handler
serialMessageReader::(Monad m, MonadIO m, MonadMask m)=>MessageHandler m ->RFXComReader (Effect (SafeT m)) ()
serialMessageReader handler = do
  env <- ask
  lift $ do
    forever $ PBS.hGetSome 1 $ serialH env
    >->
    PP.parseForever msgParser
    >->
    P.mapM_ (lift . handler)

-- |Run the pipe from RFXCom to us
processSerialPort ::
     (Monad m, MonadIO m, MonadMask m)
  => MessageHandler m -- ^The RFXCom device message handler function
  -> RFXComReader m ()
processSerialPort handler = do
  env <- ask
  lift $ runEffect . runSafeP $ do
    runRFXComReader (serialMessageReader handler) env
