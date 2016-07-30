{-# OPTIONS_HADDOCK ignore-exports #-}
-- |This module contains the abstract interface for the logger functions.
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.System.Log (
  Handle(..),
  Priority(..),
  debug,
  info,
  warning,
  RFXCom.System.Log.error
  ) where

--
-- External Import section
--

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
newtype Handle = Handle { log :: Priority -> String -> IO ()}

-- |Log a debug message.
debug::Handle -- ^The handle to the logger service
        ->String -- ^The text that is written to the log
        ->IO ()
debug  = (`RFXCom.System.Log.log` Debug)


-- |Log an informational message
info::Handle -- ^The handle to the logger service
       ->String -- ^The text that is written to the log
       ->IO ()
info    = (`RFXCom.System.Log.log` Info)


-- |Log a warning message
warning::Handle -- ^The handle to the logger service
          ->String -- ^The text that is written to the log
          ->IO ()
warning = (`RFXCom.System.Log.log` Warning)


-- |Log an error message
error::Handle -- ^The handle to the logger service
        ->String -- ^The text that is written to the log
        ->IO ()
error   = (`RFXCom.System.Log.log` Error)

