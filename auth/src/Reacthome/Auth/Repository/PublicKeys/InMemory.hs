module Reacthome.Auth.Repository.PublicKeys.InMemory where

import Control.Concurrent
import Reacthome.Auth.Domain.PublicKey
import Reacthome.Auth.Domain.PublicKeys
import Util.MVar

makePublicKeys :: IO PublicKeys
makePublicKeys = do
    list <- newMVar []

    let getAll = readMVar list

        store key = runModify list (key :)

        cleanUp t = runModify list $
            filter
                \key -> key.timestamp > t
    pure
        PublicKeys
            { getAll
            , store
            , cleanUp
            }
