module Util.Timer where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import Data.IORef (modifyIORef', newIORef, readIORef)

newtype Timer = Timer {ticks :: IO Int}

makeTimer :: Int -> IO Timer
makeTimer usInterval = do
    timer <- newIORef 0

    void $ forkIO do
        modifyIORef' timer (+ 1)

    threadDelay usInterval

    pure
        Timer
            { ticks = readIORef timer
            }
