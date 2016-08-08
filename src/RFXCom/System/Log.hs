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
  _info,
  _debug,
  _warning,
  _error
  ) where

--
-- External Import section
--
import Prelude hiding (log)
import qualified System.IO as SIO
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)

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
data Handle = Handle { log :: Priority -> String -> IO ()}

--
-- The class
--
class (Monad m, MonadIO m) => MonadLogger m where
  
  info::String->m ()
  warning::String->m ()
  error::String->m ()
  debug::String->m ()
  
--
-- The actual type definition of the logger transfomer monad
--
newtype LoggerT m a = LoggerT (ReaderT Handle m a)
  deriving (Functor, Applicative, Monad, MonadIO)

--
-- The concrete implementation of the MonadLogger class
--
instance (Monad m, MonadIO m)=>MonadLogger (LoggerT m) where
  
  info s = logger Info s  
  warning s = logger Warning s
  error s = logger Error s
  debug s = logger Debug s

--
-- Injects the environment into the LoggerT
--
runLoggerT::LoggerT m a->Handle-> m a
runLoggerT (LoggerT f) h = runReaderT f h  

logger::(Monad m, MonadIO m)=>Priority->String->LoggerT m ()
logger p s = do
  handle <- LoggerT ask
  liftIO $ (log handle) p s

--
-- The old ones
--

_debug::Handle->String->IO ()
_debug = (`log` Debug)

_info::Handle->String->IO ()
_info = (`log` Info)

_error::Handle->String->IO ()
_error = (`log` Error)

_warning::Handle->String->IO ()
_warning = (`log` Warning)
