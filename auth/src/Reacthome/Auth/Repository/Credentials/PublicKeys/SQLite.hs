module Reacthome.Auth.Repository.Credentials.PublicKeys.SQLite where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import Data.Foldable (traverse_)
import Data.Maybe
import Data.Pool
import Data.Text (Text)
import Data.UUID
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import GHC.Generics
import Reacthome.Auth.Domain.Credential.PublicKey
import Reacthome.Auth.Domain.Credential.PublicKey.Algorithm
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.User.Id
import Reacthome.Auth.Repository.Credentials.PublicKeys.SQLite.Query
import Util.SQLite

makePublicKeys :: Pool Connection -> IO PublicKeys
makePublicKeys pool = do
    withResource pool \connection ->
        traverse_
            (execute_ connection)
            [ createPublicKeysTable
            , createPublicKeysIndex
            ]

    let
        findById id' =
            findBy
                pool
                findPublicKeyById
                id'.value

        findByUserId uid =
            findBy'
                pool
                findPublicKeyByUserId
                (toByteString uid.value)

        store key =
            tryExecute
                pool
                storePublicKey
                (toPublicKeyRow key)

        remove kid =
            either
                print
                (const $ pure ())
                =<< runExceptT
                    ( tryExecute
                        pool
                        removePublicKey
                        (Only kid.value)
                    )

    pure
        PublicKeys
            { findById
            , findByUserId
            , store
            , remove
            }

data PublicKeyRow = PublicKeyRow
    { id :: ByteString
    , user_id :: ByteString
    , algorithm :: Text
    , bytes :: ByteString
    }
    deriving stock (Generic, Eq, Show)
    deriving anyclass (FromRow, ToRow)

fromPublicKeyRow :: PublicKeyRow -> Maybe PublicKey
fromPublicKeyRow key = do
    uid <- UserId <$> fromByteString key.user_id
    algorithm <- case key.algorithm of
        "ED25519" -> Just ED25519
        "ES256" -> Just ES256
        "RS256" -> Just RS256
        _ -> Nothing
    pure
        PublicKey
            { id = PublicKeyId $ toStrict key.id
            , userId = uid
            , algorithm
            , bytes = toStrict key.bytes
            }

toPublicKeyRow :: PublicKey -> PublicKeyRow
toPublicKeyRow key =
    PublicKeyRow
        { id = fromStrict key.id.value
        , user_id = toByteString key.userId.value
        , algorithm = case key.algorithm of
            ED25519 -> "ED25519"
            ES256 -> "ES256"
            RS256 -> "RS256"
        , bytes = fromStrict key.bytes
        }

findBy ::
    (ToField p) =>
    Pool Connection ->
    Query ->
    p ->
    MaybeT IO PublicKey
findBy pool q p = do
    res <- lift . runExceptT $ tryQuery pool q (Only p)
    case res of
        Left e -> do
            lift $ print e
            hoistMaybe Nothing
        Right [] -> hoistMaybe Nothing
        Right (user : _) -> hoistMaybe $ fromPublicKeyRow user

findBy' ::
    (ToField p) =>
    Pool Connection ->
    Query ->
    p ->
    IO [PublicKey]
findBy' pool q p = do
    res <- runExceptT $ tryQuery pool q (Only p)
    case res of
        Left e -> do
            print e
            pure []
        Right rows -> do
            let users = fromPublicKeyRow <$> rows
                valid = filter (/= Nothing) users
            pure $ fromJust <$> valid
