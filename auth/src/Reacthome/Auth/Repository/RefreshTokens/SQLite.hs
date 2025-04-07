module Reacthome.Auth.Repository.RefreshTokens.SQLite where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import Data.Pool
import Data.UUID
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import GHC.Generics
import Reacthome.Auth.Domain.Challenge
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
        findByToken token =
            findBy
                pool
                findRefreshTokenByToken
                token.value

        store refresh = do
            tryExecute
                pool
                storeRefreshToken
                (toRefreshTokenRow refresh)

        remove refresh =
            either
                print
                (const $ pure ())
                =<< runExceptT
                    ( tryExecute
                        pool
                        removeRefreshToken
                        (Only refresh.token.value)
                    )

    pure
        RefreshTokens
            { findByToken
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
            , token = makeChallenge $ toStrict row.token
            }

toRefreshTokenRow :: RefreshToken -> RefreshTokenRow
toRefreshTokenRow refresh =
    RefreshTokenRow
        { user_id = toByteString refresh.userId.value
        , token = fromStrict refresh.token.value
        }

findBy ::
    (ToField p) =>
    Pool Connection ->
    Query ->
    p ->
    MaybeT IO RefreshToken
findBy pool q p = do
    res <- lift . runExceptT $ tryQuery pool q (Only p)
    case res of
        Left e -> do
            lift $ print e
            hoistMaybe Nothing
        Right [] -> hoistMaybe Nothing
        Right (row : _) -> hoistMaybe $ fromRefreshTokenRow row
