module Reacthome.Relay.Repository where

import Control.Concurrent.STM (atomically)
import Data.UUID (UUID)
import ListT (toList)
import Reacthome.Relay (Relay)
import StmContainers.Multimap (delete, insert, listTByKey, newIO)

data Repository = Repository
    { add :: UUID -> Relay -> IO ()
    , remove :: UUID -> Relay -> IO ()
    , get :: UUID -> IO [Relay]
    }

makeRepository :: IO Repository
makeRepository = do
    repository <- newIO

    let
        add uid relay = atomically $ insert relay uid repository
        remove uid relay = atomically $ delete relay uid repository
        get uid = atomically $ toList $ listTByKey uid repository

    pure Repository{..}
