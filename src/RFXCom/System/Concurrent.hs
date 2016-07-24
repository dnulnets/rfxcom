{-# OPTIONS_HADDOCK ignore-exports #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
-- |This module contains global functions and data structures.
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.System.Concurrent (
  waitForChildren,
  forkChild
  ) where

--
-- External Import section
--
import           Control.Concurrent
import           Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar,
                                          takeMVar, withMVar)

import           System.IO
import           System.IO.Unsafe        (unsafePerformIO)

--
-- Internal Import Section
--

-- |Holds a list of all threads in this application. This list is not made public outside
-- this module to ensure no one is tampering with it.
threadChildren::MVar [MVar ()]
{-# NOINLINE threadChildren #-}
threadChildren = unsafePerformIO (newMVar [])

-- |Wait for all threads to end before returning.
waitForChildren::IO ()
waitForChildren = do
  putStrLn $ "Waiting for thread to finish"
  cs <- takeMVar threadChildren
  case cs of
    []   -> return ()
    m:ms -> do
      putMVar threadChildren ms
      takeMVar m
      waitForChildren

-- |Forks a thread and add it to the global list of threads and make sure it
-- removes itself when it is finished or otherwise killed.
forkChild::IO ()       -- ^The IO action to be executed
         ->IO ThreadId -- ^The thread id of the thread
forkChild io = do
  mvar <- newEmptyMVar
  childs <- takeMVar threadChildren
  putMVar threadChildren (mvar:childs)
  putStrLn "Starting thread"
  forkFinally io (\_ -> putMVar mvar ())
