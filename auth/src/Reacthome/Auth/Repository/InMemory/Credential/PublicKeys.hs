module Reacthome.Auth.Repository.InMemory.Credential.PublicKeys where

import Control.Concurrent.MVar
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict as M
import Data.HashSet as S
import Data.Maybe
import Reacthome.Auth.Domain.Credential.PublicKey
import Reacthome.Auth.Domain.Credential.PublicKeys
import Util.MVar

makePublicKeys :: IO PublicKeys
makePublicKeys = do
    map' <- newMVar (M.empty, M.empty)
    let
        findById id' =
            MaybeT $ runRead
                map'
                \(byId, _) -> M.lookup id' byId

        findByUserId id' =
            MaybeT $ runRead
                map'
                \(_, byUser) -> S.toList <$> M.lookup id' byUser

        store publicKey =
            ExceptT $
                modifyMVar
                    map'
                    \value@(byId, byUserId) -> do
                        let existingPublicKey = M.lookup publicKey.id byId
                        if isJust existingPublicKey
                            then pure (value, Left "Public key already exists")
                            else do
                                let publicKeys' =
                                        case M.lookup publicKey.userId byUserId of
                                            Just publicKeys ->
                                                S.insert publicKey publicKeys
                                            Nothing ->
                                                S.singleton publicKey
                                pure
                                    (
                                        ( M.insert publicKey.id publicKey byId
                                        , M.insert publicKey.userId publicKeys' byUserId
                                        )
                                    , Right ()
                                    )

        remove id' =
            modifyMVar
                map'
                \(byId, byUser) -> do
                    pure
                        ( case M.lookup id' byId of
                            Just publicKey ->
                                case M.lookup publicKey.userId byUser of
                                    Just publicKeys ->
                                        ( M.delete id' byId
                                        , M.insert publicKey.userId (S.delete publicKey publicKeys) byUser
                                        )
                                    Nothing ->
                                        ( M.delete id' byId
                                        , byUser
                                        )
                            Nothing ->
                                ( M.delete id' byId
                                , byUser
                                )
                        , ()
                        )
    pure
        PublicKeys
            { findById
            , findByUserId
            , store
            , remove
            }
