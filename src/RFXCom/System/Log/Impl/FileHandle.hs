{-# OPTIONS_HADDOCK ignore-exports #-}
-- |This module contains the concrete implementation of the logger functionality
-- to a filehandle.
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.System.Log.Impl.FileHandle (
  withHandle,
  Config(..)
  ) where

--
-- External Import section
--
import           Control.Concurrent       (forkIO)
import           Control.Concurrent.MVar  (MVar, newEmptyMVar, newMVar, putMVar,
                                           takeMVar, withMVar)
import           Data.Monoid              ((<>))
import qualified Data.Text                as T (Text, unpack)
import qualified System.IO                as SIO (Handle, hPutStrLn)

--
-- Internal Import Section
--
import           RFXCom.System.Concurrent (forkChild)
import qualified RFXCom.System.Log        as Log (Handle (..), Priority (..))

-- |Configuration for the file handler logger.
data Config = Config

data Message = Message Log.Priority String
             | Stop (MVar ())

logMessage::MVar Message->Log.Priority->String->IO ()
logMessage mvar p t = putMVar mvar (Message p t)

-- | Run an 'IO' action with access to an 'X.Handle'.
withHandle
    :: Config
    -> (Log.Handle -> IO a)
    -> IO a
withHandle config io = do
  m <- newEmptyMVar
  tid <- forkChild $ loggerThread m
  x <- io $ Log.Handle { Log.log =  logMessage m }
  putStrLn "C"
  s <- stopLoggerThread m
  putStrLn "D"
  return x

--
loggerThread::MVar Message -> IO ()
loggerThread m = do
  putStrLn "Logger:Started"
  loop
  where
    loop = do
      cmd <- takeMVar m
      case cmd of
        Message p t -> do
          putStrLn $ (show p) ++ " " ++ (show t)
          loop
        Stop s -> do
          putStrLn "Logger:Stoped"
          putMVar s ()

stopLoggerThread::MVar Message -> IO()
stopLoggerThread m = do
  s<-newEmptyMVar
  putMVar m (Stop s)
  takeMVar s
