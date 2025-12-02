module Reacthome.Relay.Stat where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Prelude hiding (length, splitAt, tail)

data RelayHits = RelayHits
    { hits :: IO Int
    , hit :: Int -> IO ()
    }

makeRelayHits :: IO RelayHits
makeRelayHits = do
    counter <- newIORef 0
    pure
        RelayHits
            { hits = readIORef counter
            , hit = modifyIORef' counter <$> (+)
            }

data RelayStat = RelayStat
    { rx :: !RelayHits
    , tx :: !RelayHits
    , hits :: IO RelayStatHits
    }

data RelayStatHits = RelayStatHits
    { rx :: Int
    , tx :: Int
    }

makeRelayStat :: IO RelayStat
makeRelayStat = do
    rx <- makeRelayHits
    tx <- makeRelayHits
    let hits = liftA2 RelayStatHits rx.hits tx.hits
    pure RelayStat{..}
