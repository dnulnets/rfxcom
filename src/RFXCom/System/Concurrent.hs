{-# OPTIONS_HADDOCK ignore-exports #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

-- |This module contains global functions and data structures for dealing with
-- concurrency within RFXCom.
--
-- Written by Tomas Stenlund, Sundsvall, Sweden, 2016-02-06
-- Copyright (c) 2017, Sundsvall, Sweden.
-- See LICENSE file.
--
module RFXCom.System.Concurrent (
  waitForChildren,
  forkChild,
  executeAfterDelay
  ) where

--
-- External Import section
--
import           Control.Concurrent      (ThreadId, forkFinally, killThread,
                                          myThreadId, forkIO, threadDelay)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar,
                                          takeMVar)

import           System.IO
import           System.IO.Unsafe        (unsafePerformIO)

--
-- Internal Import Section
--

-- |Éxecutes an IO action after a certain amount of time
executeAfterDelay :: Int    -- ^The delay in microseconds 
                  -> IO ()  -- ^The IO action to execute after the delay
                  -> IO ThreadId -- ^The threadId of the started thread
executeAfterDelay t f = forkIO (threadDelay t >> f)

-- |Holds a list of all threads in this application. This list is not made public outside
-- this module to ensure no one is tampering with it.
threadChildren::MVar [(ThreadId, MVar ())]
{-# NOINLINE threadChildren #-}
threadChildren = unsafePerformIO (newMVar [])


-- |Wait for all threads to end before returning.
waitForChildren::IO ()
waitForChildren = do
  cs <- takeMVar threadChildren
  putMVar threadChildren cs
  loop
  where

    -- Wait for each thread in the array to finish by waiting for its MVar that it has finished.
    loop = do
      cs <- takeMVar threadChildren
      case cs of
        []   -> return ()
        m:ms -> do
          putMVar threadChildren ms
          takeMVar $ snd m
          loop

-- |Forks a thread and add it to the global list of threads and make sure it
-- removes itself when it is finished or otherwise killed.
forkChild::IO ()       -- ^The IO action to be executed
         ->IO ThreadId -- ^The id of the thread
forkChild io = do
  mvar <- newEmptyMVar
  childs <- takeMVar threadChildren
  tid <- forkFinally io (\_ -> putMVar mvar ())
  putMVar threadChildren ((tid,mvar):childs)
  return tid
