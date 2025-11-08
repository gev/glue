module Reacthome.Relay.Repository where

import Control.Concurrent.STM (atomically)
import Data.UUID (UUID)
import ListT (toList)
import Reacthome.Relay.Connection (RelayConnection)
import StmContainers.Multimap (delete, insert, listTByKey, newIO)

data ConnectionRepository = ConnectionRepository
    { add :: UUID -> RelayConnection -> IO ()
    , remove :: UUID -> RelayConnection -> IO ()
    , get :: UUID -> IO [RelayConnection]
    }

makeRepository :: IO ConnectionRepository
makeRepository = do
    repository <- newIO

    let
        add uid relay = atomically $ insert relay uid repository
        remove uid relay = atomically $ delete relay uid repository
        get uid = atomically $ toList $ listTByKey uid repository

    pure ConnectionRepository{..}
