module Reacthome.Relay.Repository where

import Control.Concurrent.STM (atomically)
import Data.ByteString (ByteString)
import ListT (toList)
import Reacthome.Relay.Relay (Relay)
import StmContainers.Multimap (delete, insert, listTByKey, newIO)

data RelayRepository = RelayRepository
    { add :: ByteString -> Relay -> IO ()
    , remove :: ByteString -> Relay -> IO ()
    , get :: ByteString -> IO [Relay]
    }

makeRepository :: IO RelayRepository
makeRepository = do
    repository <- newIO

    let
        add uid relay = atomically $ insert relay uid repository
        remove uid relay = atomically $ delete relay uid repository
        get uid = atomically $ toList $ listTByKey uid repository

    pure RelayRepository{..}
