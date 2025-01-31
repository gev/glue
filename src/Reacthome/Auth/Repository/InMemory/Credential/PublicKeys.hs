module Reacthome.Auth.Repository.InMemory.Credential.PublicKeys where

import Control.Concurrent.MVar
import Data.HashMap.Strict
import Reacthome.Auth.Domain.Credential.PublicKey
import Reacthome.Auth.Domain.Credential.PublicKeys
import Util.MVar
import Prelude hiding (lookup)

mkPublicKeys :: IO PublicKeys
mkPublicKeys = do
    map' <- newMVar empty
    let
        get id' =
            runRead
                map'
                \publicKeys ->
                    case lookup id' publicKeys of
                        (Just publicKey) -> Right publicKey
                        _ -> Left $ "Public key not found " <> show id'

        store publicKey =
            modifyMVar
                map'
                $ \publicKeys ->
                    pure case lookup publicKey.id publicKeys of
                        (Just _) ->
                            ( publicKeys
                            , Left "Public key already exists"
                            )
                        _ ->
                            ( insert publicKey.id publicKey publicKeys
                            , Right ()
                            )

        remove = runModify map' . delete
    pure
        PublicKeys
            { get
            , store
            , remove
            }
