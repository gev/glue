module Util.Channel where

import Control.Concurrent.STM (atomically, flushTBQueue, newTBQueueIO, readTBQueue, writeTBQueue)

data Channel t = Channel
    { read :: IO t
    , flush :: IO [t]
    , write :: t -> IO ()
    }

makeQueue :: Int -> IO (Channel t)
makeQueue bound = do
    queue <- newTBQueueIO $ fromIntegral bound
    pure
        Channel
            { read = atomically $ readTBQueue queue
            , flush = atomically $ flushTBQueue queue
            , write = atomically . writeTBQueue queue
            }
