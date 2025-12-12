module Reacthome.Auth.Repository.PublicKeys.SQLite where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import Data.Foldable
import Data.Maybe
import Data.Pool
import Data.UUID
import Database.SQLite.Simple
import GHC.Generics
import Reacthome.Auth.Domain.PublicKey
import Reacthome.Auth.Domain.PublicKeys
import Reacthome.Auth.Repository.PublicKeys.SQLite.Query
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
        getAll = runExceptT do
            findBy pool getAllPublicKeys

        store key =
            tryExecute
                pool
                storePublicKey
                (toPublicKeyRow key)

        cleanUp t = do
            tryExecute
                pool
                cleanUpPublicKeys
                (Only t)

    pure PublicKeys{..}

data PublicKeyRow = PublicKeyRow
    { id :: ByteString
    , timestamp :: Int
    , bytes :: ByteString
    }
    deriving stock (Generic, Eq, Show)
    deriving anyclass (FromRow, ToRow)

fromPublicKeyRow :: PublicKeyRow -> Maybe PublicKey
fromPublicKeyRow row = do
    kid <- fromByteString row.id
    pure
        PublicKey
            { kid
            , timestamp = row.timestamp
            , bytes = toStrict row.bytes
            }

toPublicKeyRow :: PublicKey -> PublicKeyRow
toPublicKeyRow key =
    PublicKeyRow
        { id = toByteString key.kid
        , timestamp = key.timestamp
        , bytes = fromStrict key.bytes
        }

findBy ::
    Pool Connection ->
    Query ->
    ExceptT String IO [PublicKey]
findBy pool q = do
    rows <- except =<< lift (tryQuery_ pool q)
    let keys = fromPublicKeyRow <$> rows
        valid = filter (/= Nothing) keys
    pure $ fromJust <$> valid
