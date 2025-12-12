module Reacthome.Auth.Repository.Credentials.PublicKeys.SQLite where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
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
        findById id' = runExceptT do
            findBy pool findPublicKeyById id'.value >>= \case
                [key] -> pure key
                [] -> throwE ("Public key " <> show id'.value <> " not found")
                (_ : _) -> throwE ("Public key " <> show id'.value <> " is not unique")

        findByUserId uid = runExceptT do
            findBy pool findPublicKeyByUserId (toByteString uid.value)

        store key =
            tryExecute
                pool
                storePublicKey
                (toPublicKeyRow key)

        remove kid =
            tryExecute
                pool
                removePublicKey
                (Only kid.value)

    pure PublicKeys{..}

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
    ExceptT String IO [PublicKey]
findBy pool q p = do
    rows <- except =<< lift (tryQuery pool q $ Only p)
    let users = fromPublicKeyRow <$> rows
        valid = filter (/= Nothing) users
    pure (fromJust <$> valid)
