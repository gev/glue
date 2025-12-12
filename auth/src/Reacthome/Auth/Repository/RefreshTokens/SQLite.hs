module Reacthome.Auth.Repository.RefreshTokens.SQLite where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import Data.Maybe (fromJust)
import Data.Pool
import Data.UUID
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import GHC.Generics
import Reacthome.Auth.Domain.Hash
import Reacthome.Auth.Domain.RefreshToken
import Reacthome.Auth.Domain.RefreshTokens
import Reacthome.Auth.Domain.User.Id
import Reacthome.Auth.Environment
import Reacthome.Auth.Repository.RefreshTokens.SQLite.Query
import Util.SQLite
import Prelude hiding (lookup)

makeRefreshTokens ::
    (?environment :: Environment) =>
    Pool Connection ->
    IO RefreshTokens
makeRefreshTokens pool = do
    withResource pool (`execute_` createRefreshTokensTable)
    let
        findByHash hash = runExceptT do
            findBy pool findRefreshTokenByToken hash.value >>= \case
                [token] -> pure token
                [] -> throwE ("Refresh token " <> show hash <> " not found")
                (_ : _) -> throwE ("Refresh token " <> show hash <> " not unique")

        store token = do
            tryExecute
                pool
                storeRefreshToken
                (toRefreshTokenRow token)

        remove token =
            tryExecute
                pool
                removeRefreshToken
                (Only token.hash.value)

    pure
        RefreshTokens
            { findByHash
            , store
            , remove
            }

data RefreshTokenRow = RefreshTokenRow
    { token :: ByteString
    , user_id :: ByteString
    }
    deriving stock (Generic, Eq, Show)
    deriving anyclass (FromRow, ToRow)

fromRefreshTokenRow :: RefreshTokenRow -> Maybe RefreshToken
fromRefreshTokenRow row = do
    userId <- UserId <$> fromByteString row.user_id
    pure
        RefreshToken
            { userId
            , hash = Hash $ toStrict row.token
            }

toRefreshTokenRow :: RefreshToken -> RefreshTokenRow
toRefreshTokenRow token =
    RefreshTokenRow
        { user_id = toByteString token.userId.value
        , token = fromStrict token.hash.value
        }

findBy ::
    (ToField p) =>
    Pool Connection ->
    Query ->
    p ->
    ExceptT String IO [RefreshToken]
findBy pool q p = do
    rows <- except =<< lift (tryQuery pool q $ Only p)
    let tokens = fromRefreshTokenRow <$> rows
        valid = filter (/= Nothing) tokens
    pure $ fromJust <$> valid
