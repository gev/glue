{-# LANGUAGE DerivingVia #-}

module Reacthome.Auth.Repository.InMemory.Credential.PublicKeys where

import Control.Concurrent.MVar
import Data.HashMap.Strict as M
import Data.HashSet as S
import Data.Hashable
import Reacthome.Auth.Domain.Credential.PublicKey
import Reacthome.Auth.Domain.Credential.PublicKeys
import Util.MVar

mkPublicKeys :: IO PublicKeys
mkPublicKeys = do
    map' <- newMVar (M.empty, M.empty)
    let
        getById id' =
            runRead
                map'
                \(byId, _) ->
                    case M.lookup id' byId of
                        (Just publicKey) -> Right publicKey
                        _ -> Left $ "Public key not found " <> show id'

        getByUserId id' =
            runRead
                map'
                \(_, byUser) ->
                    case M.lookup id' byUser of
                        (Just publicKeys) -> Right $ S.toList publicKeys
                        _ -> Left $ "Public keys not found by user " <> show id'

        store publicKey =
            modifyMVar
                map'
                $ \(byId, byUserId) ->
                    pure case M.lookup publicKey.id byId of
                        (Just _) ->
                            (
                                ( byId
                                , byUserId
                                )
                            , Left "Public key already exists"
                            )
                        _ -> case M.lookup publicKey.userId byUserId of
                            (Just publicKeys) ->
                                (
                                    ( M.insert publicKey.id publicKey byId
                                    , M.insert publicKey.userId (S.insert publicKey publicKeys) byUserId
                                    )
                                , Right ()
                                )
                            _ ->
                                (
                                    ( M.insert publicKey.id publicKey byId
                                    , M.insert publicKey.userId (S.singleton publicKey) byUserId
                                    )
                                , Right ()
                                )

        remove id' =
            modifyMVar
                map'
                \(byId, byUser) -> do
                    pure case M.lookup id' byId of
                        Just publicKey ->
                            case M.lookup publicKey.userId byUser of
                                (Just publicKeys) ->
                                    (
                                        ( M.delete id' byId
                                        , M.insert publicKey.userId (S.delete publicKey publicKeys) byUser
                                        )
                                    , ()
                                    )
                                _ ->
                                    (
                                        ( M.delete id' byId
                                        , byUser
                                        )
                                    , ()
                                    )
                        _ ->
                            (
                                ( M.delete id' byId
                                , byUser
                                )
                            , ()
                            )
    pure
        PublicKeys
            { getById
            , getByUserId
            , store
            , remove
            }

instance Eq PublicKey where
    a == b = a.id == b.id

instance Hashable PublicKey where
    hashWithSalt salt publicKey =
        hashWithSalt salt publicKey.id
