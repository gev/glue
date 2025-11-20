module Reacthome.Relay.Stat where

import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Word (Word64)
import Prelude hiding (length, splitAt, tail)

data RelayHits = RelayHits
    { hits :: IO Word64
    , hit :: IO ()
    }

makeRelayHits :: IO RelayHits
makeRelayHits = do
    counter <- newIORef 0
    pure
        RelayHits
            { hits = readIORef counter
            , hit = modifyIORef counter (+ 1)
            }

data RelayStat = RelayStat
    { rx :: RelayHits
    , tx :: RelayHits
    }

makeRelayStat :: IO RelayStat
makeRelayStat = do
    rx <- makeRelayHits
    tx <- makeRelayHits
    pure RelayStat{..}
