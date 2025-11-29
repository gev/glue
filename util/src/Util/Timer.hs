{-# LANGUAGE RecordWildCards #-}

module Util.Timer where

import Control.Concurrent (forkIO, threadDelay, yield)
import Control.Monad (forever, void, when)
import Data.IORef (modifyIORef', newIORef, readIORef)

newtype TimerManager = TimerManager {makeTimer :: IO Timer}
newtype Timer = Timer {wait :: Int -> IO ()}

makeTimerManager :: Int -> IO TimerManager
makeTimerManager usAccuracy = do
    time <- newIORef 0
    let
        makeTimer = do
            let
                wait usDelay = do
                    now <- readIORef time
                    let deadLine = now + usDelay
                    let loop = do
                            now' <- readIORef time
                            when (now' < deadLine) do
                                yield
                                loop
                    loop
            pure Timer{..}

    void . forkIO $ forever do
        modifyIORef' time (+ usAccuracy)
        threadDelay usAccuracy

    pure TimerManager{..}
