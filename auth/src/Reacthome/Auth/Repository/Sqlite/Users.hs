module Reacthome.Auth.Repository.SQLite.Users where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (traverse_)
import Data.Pool
import Data.Text (Text)
import Data.UUID hiding (null)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import GHC.Generics
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.User.Id
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.User.Name
import Reacthome.Auth.Domain.User.Status
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Repository.SQLite.Users.Query
import Util.SQLite

makeUsers :: Pool Connection -> IO Users
makeUsers pool = do
    withResource pool \connection ->
        traverse_
            (execute_ connection)
            [ createUsersTable
            , createUsersIndex
            ]

    let
        findById uid =
            findBy
                pool
                findUserById
                $ toByteString uid.value

        findByLogin login =
            findBy
                pool
                findUserByLogin
                login.value

        has login =
            either
                ( \e -> do
                    print e
                    pure False
                )
                ( \case
                    [Only count] -> pure $ count > (0 :: Int)
                    _ -> pure False
                )
                =<< runExceptT
                    ( tryQuery
                        pool
                        countUserByLogin
                        (Only login.value)
                    )

        store user =
            tryExecute
                pool
                storeUser
                (toUserRow user)

        remove user =
            either
                print
                (const $ pure ())
                =<< runExceptT
                    ( tryExecute
                        pool
                        removeUser
                        (Only $ toByteString user.id.value)
                    )

    pure
        Users
            { findById
            , findByLogin
            , has
            , store
            , remove
            }

data UserRow = UserRow
    { id :: ByteString
    , login :: Text
    , name :: Text
    , status :: Text
    }
    deriving stock (Generic, Eq, Show)
    deriving anyclass (FromRow, ToRow)

fromUserRow :: UserRow -> Maybe User
fromUserRow user = do
    uid <- UserId <$> fromByteString user.id
    status <- case user.status of
        "active" -> Just Active
        "inactive" -> Just Inactive
        "suspended" -> Just Suspended
        _ -> Nothing
    pure
        User
            { id = uid
            , login = UserLogin user.login
            , name = UserName user.name
            , status
            }

toUserRow :: User -> UserRow
toUserRow user =
    UserRow
        { id = toByteString user.id.value
        , login = user.login.value
        , name = user.name.value
        , status = case user.status of
            Active -> "active"
            Inactive -> "inactive"
            Suspended -> "suspended"
        }

findBy ::
    (ToField p) =>
    Pool Connection ->
    Query ->
    p ->
    MaybeT IO User
findBy pool q p = do
    res <- lift . runExceptT $ tryQuery pool q (Only p)
    case res of
        Left e -> do
            lift $ print e
            hoistMaybe Nothing
        Right [] -> hoistMaybe Nothing
        Right (user : _) -> hoistMaybe $ fromUserRow user
