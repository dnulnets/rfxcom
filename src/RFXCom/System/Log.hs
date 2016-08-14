{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |This module contains the abstract interface for the logger functions.
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.System.Log (
  Handle(..),
  
  Priority(..),
  
  MonadLogger(..),
  LoggerT(..),
  runLoggerT,
  
  _info,
  _debug,
  _warning,
  _error
  ) where

--
-- External Import section
--
import Prelude hiding (log, error)
import qualified System.IO as SIO
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (StateT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch   (MonadMask, MonadCatch, MonadThrow)
import Control.Monad.Trans.Class (MonadTrans(..))

--
-- Internal Import Section
--

-- |The priority of the log message
data Priority
    = Debug    -- ^Debug messages
    | Info     -- ^Notable information that requires no immediate action.
    | Warning  -- ^Something is probably wrong, and we should investigate.
    | Error    -- ^Something is wrong and immediate action is required.
    deriving (Eq, Ord, Show)

-- |The handle to the logger services.
data Handle = Handle { writeLog :: Priority -> String -> IO ()}


-- |The MonadLogger class that defines the logger functions for different priority levels and makes them
-- easily accesible.
class (Monad m, MonadIO m) => MonadLogger m where

  -- |Writes an informational message to the log
  info::String -- ^The informational message
      ->m ()

  -- |Writes a warning message to the log
  warning::String -- ^The warning message
         ->m ()

  -- |Writes an error message to the log
  error::String
       ->m () -- ^The error message

  -- |Writes a debug message to the log
  debug::String -- ^The debug message
       ->m ()


-- |The type definition of the logger transformer
newtype LoggerT m a = LoggerT ((ReaderT Handle m) a)
  deriving (Functor, Applicative, Monad, MonadReader Handle, MonadIO, MonadCatch, MonadMask, MonadThrow)


-- |The concrete implementation of the MonadTrans class for the logger transformer
instance MonadTrans LoggerT where
  lift m = LoggerT $ lift m

  
-- |The concrete implementation of the MonadLogger class for the logger transformer
instance (Monad m, MonadIO m)=>MonadLogger (LoggerT m) where
  
  info s = write Info s
  warning s = write Warning s
  error s = write Error s
  debug s = write Debug s
  

-- |Injects the environment into the LoggerT and run the computations
runLoggerT::LoggerT m a   -- ^The logger monad
          ->Handle-> m a  -- ^The environment
runLoggerT (LoggerT m) h = runReaderT m h  

-- |The raw logger function that sends the priority and message to the logger
write::(Monad m,MonadIO m)=>Priority -- ^The priority of the message
     ->String                        -- ^The message
     ->LoggerT m ()
write p s = do
  handle <- LoggerT ask
  liftIO $ (writeLog handle) p s

--
-- The old ones, to be removed
--
_debug::Handle->String->IO ()
_debug = (`writeLog` Debug)

_info::Handle->String->IO ()
_info = (`writeLog` Info)

_error::Handle->String->IO ()
_error = (`writeLog` Error)

_warning::Handle->String->IO ()
_warning = (`writeLog` Warning)

--
-- The mtl style transformer setup for the logger
--

-- |The instance of the MonadLogger class for the reader transformer.
instance (MonadLogger m) => MonadLogger (ReaderT r m) where
  info = lift . info
  debug = lift . debug
  error = lift . error
  warning = lift . warning

-- |The instance of the MonadLogger class for the reader transformer.
instance (MonadLogger m) => MonadLogger (StateT s m) where
  info = lift . info
  debug = lift . debug
  error = lift . error
  warning = lift . warning
  
