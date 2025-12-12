module Reacthome.Auth.Repository.PublicKeys.InMemory where

import Control.Concurrent
import Reacthome.Auth.Domain.PublicKey
import Reacthome.Auth.Domain.PublicKeys
import Util.MVar

makePublicKeys :: IO PublicKeys
makePublicKeys = do
    list <- newMVar []
    let
        getAll = Right <$> readMVar list
        store key = Right <$> runModify list (key :)
        cleanUp t = Right <$> runModify list (filter \key -> key.timestamp > t)
    pure PublicKeys{..}
