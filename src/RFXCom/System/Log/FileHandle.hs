{-# OPTIONS_HADDOCK ignore-exports #-}
-- |This module contains the concrete implementation of the logger functionality
-- to a filehandle.
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.System.Log.FileHandle (
  withHandle,
  Config(..)
  ) where

--
-- External Import section
--
import           Control.Concurrent       (ThreadId, myThreadId)
import           Control.Concurrent.MVar  (MVar, newEmptyMVar, newMVar, putMVar,
                                           takeMVar)

import           Data.Time.LocalTime      (getZonedTime)

import qualified System.IO                as SIO (Handle, hPutStrLn)

--
-- Internal Import Section
--
import           RFXCom.System.Concurrent (forkChild)
import qualified RFXCom.System.Log        as Log (Handle (..), Priority (..))

-- |Configuration for the file handler logger.
data Config = Config {
  logName::String -- ^Name of the logfile, will generate <name>.log file
  } deriving (Show)

-- |Message sent to the logger thread, containing either a message or a stop command
data Message = Message ThreadId Log.Priority String -- ^A log message to the logger thread, containnig the thread id, priority and the text.
             | Stop (MVar ())                       -- ^A stop message to the logger thread

-- |An internal function that sends a message to the logger thread that it should log a message
logMessage::MVar Message -- ^The communication MVar to the thread
          ->Log.Priority -- ^The priority of the log message
          ->String       -- ^The text of the log message
          ->IO ()
logMessage mvar p t = do
  tid <- myThreadId
  putMVar mvar (Message tid p t)

-- |The logger thread that waits for messages containing log entries or a stop command to
-- stop the thread.
loggerThread::MVar Message -> IO ()
loggerThread m = do
  putStrLn "Logger:Started"
  loop
  where
    loop = do
      cmd <- takeMVar m
      case cmd of
        Message tid p t -> do
          zt <- getZonedTime
          putStrLn $ (show zt) ++ " " ++ (show tid) ++ " " ++ (show p) ++ ":" ++ (show t)
          loop
        Stop s -> do
          putStrLn "Logger:Stoped"
          putMVar s ()

-- |Sends a stop message the logger thread and waits for it to have finished.
stopLoggerThread::MVar Message -> IO()
stopLoggerThread m = do
  s<-newEmptyMVar
  putMVar m (Stop s)
  takeMVar s

-- | Run an 'IO' action with the logger function. Note that for each withHandle a new logger thread will be created
-- and the logger thread will be stopped once the action has been executed.
withHandle
    :: Config                 -- ^The configuration of the logger
    -> (Log.Handle -> IO a)   -- ^The IO action
    -> IO a
withHandle config io = do
  m <- newEmptyMVar
  tid <- forkChild $ loggerThread m
  x <- io $ Log.Handle { Log.log =  logMessage m }
  s <- stopLoggerThread m
  return x

