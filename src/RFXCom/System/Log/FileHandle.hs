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
import           Control.Exception        (SomeException (..), try)

import           Data.List
import           Data.Maybe
import           Data.Time.Format         (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime      (getZonedTime, zonedTimeToUTC)
import           Text.Read                (readMaybe)

import           System.Directory
import           System.Posix.Files       (fileSize, getFileStatus)
import           System.Posix.Types       (FileOffset)

import qualified System.IO                as SIO (Handle, IOMode (..),
                                                  hPutStrLn, withFile)
import           System.IO.Error

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

-- |The name of the log file.
logFileName::String -- ^Name of the log file
           ->String -- ^Name if the log file but with correct extension
logFileName name = name ++ ".log"

-- |The log filename before the index.
logFileNameBeforeIndex::String -- ^The name of the log file
                      ->String -- ^The prefix before the index (extension) of the log file
logFileNameBeforeIndex name = name ++ ".log."

-- |Creates a name of the log file based on the name of the log file and current index.
logFileNameWithIndex::String -- ^Name of the log file
                    ->Int    -- ^Index of the file name
                    ->String -- ^File name with index as suffix (extension)
logFileNameWithIndex name ix = logFileNameBeforeIndex name ++ show ix

-- |Returns with the size in bytes of the file
getFileSize::FilePath -- ^Name of the file
           ->IO Int   -- ^The size in bytes
getFileSize path = do
  size <- try (fromIntegral . fileSize <$> getFileStatus path)::IO (Either SomeException Int)
  case size of
    Left ex -> return 0
    Right n -> return n

-- |Finds out the highest index of all log files. This is to get hold of the next log file
-- to generate when the current logfile reaches maximum size.
getHighestIndex::String -- ^Name of the log file
               ->IO Int -- ^Highest index of the log file
getHighestIndex name = do
  ix <- try (highestIndex <$> filter (isLogFile name) <$> getDirectoryContents ".")::IO (Either SomeException Int)
  case ix of
    Left ex -> return 0
    Right n -> return n
  where
    highestIndex ls = maximum $ 0:(numericIndices ls)
    numericIndices ls = catMaybes $ readMaybe <$> indices ls
    indices ls = catMaybes $ stripPrefix (logFileNameBeforeIndex name) <$> ls
    isLogFile name fname = isPrefixOf (logFileNameBeforeIndex name) fname

-- |The logger thread that waits for messages containing log entries or a stop command to
-- stop the thread.
loggerThread::SIO.Handle
            ->MVar Message
            ->IO ()
loggerThread fh m = do
  putStrLn "Logger:Started"
  loop
  where
    loop = do
      cmd <- takeMVar m
      case cmd of
        Message tid p t -> do
          utc <- zonedTimeToUTC <$> getZonedTime
          SIO.hPutStrLn fh $ (formatTime defaultTimeLocale "%FT%T.%qZ" utc) ++ "|" ++ (show tid) ++ "|" ++ (show p) ++ "|" ++ (show t)
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
  size <- getFileSize $ logName config ++ ".log"
  hix <- getHighestIndex $ logName config
  putStrLn $ "Filesize=" ++ (show size)
  putStrLn $ "Highets rotation=" ++ (show hix)
  SIO.withFile (logName config ++ ".log") SIO.AppendMode (\hF -> do
                                           m <- newEmptyMVar
                                           tid <- forkChild $ loggerThread hF m
                                           x <- io $ Log.Handle { Log.log =  logMessage m }
                                           s <- stopLoggerThread m
                                           return x
                                       )

