module Reacthome.Auth.Repository.PublicKeys.SQLite where

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
        getAll = findBy pool getAllPublicKeys

        store key =
            either
                print
                (const $ pure ())
                =<< runExceptT
                    ( tryExecute
                        pool
                        storePublicKey
                        (toPublicKeyRow key)
                    )

        cleanUp t =
            either
                print
                (const $ pure ())
                =<< runExceptT
                    ( tryExecute
                        pool
                        cleanUpPublicKeys
                        (Only t)
                    )

    pure
        PublicKeys
            { getAll
            , store
            , cleanUp
            }

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
    IO [PublicKey]
findBy pool q = do
    res <- runExceptT $ tryQuery_ pool q
    case res of
        Left e -> do
            print e
            pure []
        Right rows -> do
            let keys = fromPublicKeyRow <$> rows
                valid = filter (/= Nothing) keys
            pure $ fromJust <$> valid
