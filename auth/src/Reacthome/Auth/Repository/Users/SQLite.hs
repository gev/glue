module Reacthome.Auth.Repository.Users.SQLite where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (traverse_)
import Data.Maybe
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
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Repository.Users.SQLite.Query
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
        getAll = runExceptT do
            findAll pool getAllUsers

        findById uid = runExceptT do
            findBy pool findUserById (toByteString uid.value) >>= \case
                [user] -> pure user
                [] -> throwE ("User " <> show uid.value <> " not found")
                (_ : _) -> throwE ("User " <> show uid.value <> " is not unique")

        findByLogin login = runExceptT do
            findBy pool findUserByLogin login.value >>= \case
                [user] -> pure user
                [] -> throwE ("User " <> show login.value <> " not found")
                (_ : _) -> throwE ("User " <> show login.value <> " is not unique")

        has login = runExceptT do
            lift (tryQuery pool countUserByLogin $ Only login.value) >>= except >>= \case
                [Only count] -> pure $ count > (0 :: Int)
                _ -> pure False

        store user =
            tryExecute
                pool
                storeUser
                (toUserRow user)

        remove user =
            tryExecute
                pool
                removeUser
                (Only $ toByteString user.id.value)

    pure Users{..}

data UserRow = UserRow
    { id :: ByteString
    , login :: Text
    , name :: Text
    }
    deriving stock (Generic, Eq, Show)
    deriving anyclass (FromRow, ToRow)

fromUserRow :: UserRow -> Maybe User
fromUserRow user = do
    uid <- UserId <$> fromByteString user.id
    pure
        User
            { id = uid
            , login = UserLogin user.login
            , name = UserName user.name
            }

toUserRow :: User -> UserRow
toUserRow user =
    UserRow
        { id = toByteString user.id.value
        , login = user.login.value
        , name = user.name.value
        }

findAll ::
    Pool Connection ->
    Query ->
    ExceptT String IO [User]
findAll pool q = do
    rows <- except =<< lift (tryQuery_ pool q)
    let users = fromUserRow <$> rows
        valid = filter (/= Nothing) users
    pure $ fromJust <$> valid

findBy ::
    (ToField p) =>
    Pool Connection ->
    Query ->
    p ->
    ExceptT String IO [User]
findBy pool q p = do
    rows <- except =<< lift (tryQuery pool q $ Only p)
    let users = fromUserRow <$> rows
        valid = filter (/= Nothing) users
    pure $ fromJust <$> valid
