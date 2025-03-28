module Reacthome.Auth.Repository.Sqlite.Users where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy (ByteString)
import Data.Foldable
import Data.Text (Text)
import Data.UUID
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import Database.SQLite.Simple.ToField
import GHC.Generics
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.User.Id
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.User.Name
import Reacthome.Auth.Domain.User.Status
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Repository.Sqlite

makeUsers :: (?environment :: Environment) => IO Users
makeUsers = do
    traverse_ (execute_ ?environment.db) initUsers
    let
        findById uid =
            findBy
                [sql| 
                    SELECT id, login, status 
                    FROM users WHERE id = ? 
                    |]
                $ toByteString uid.value

        findByLogin login =
            findBy
                [sql| 
                    SELECT id, login, status 
                    FROM users WHERE login = ?
                    |]
                login.value

        has login = do
            [Only count] <-
                onlyQuery
                    [sql| 
                        SELECT COUNT(*) 
                        FROM users WHERE login = ? 
                        |]
                    login.value
            pure (count > (0 :: Int))

        store user =
            tryExecute
                [sql| 
                    REPLACE INTO users (id, login, name, status)
                    VALUES (?, ?, ?, ?)
                    |]
                $ toUserRow user

        remove user = do
            onlyExecute
                [sql| 
                    DELETE FROM users 
                    WHERE id = ? 
                    |]
                $ toByteString user.id.value

    pure
        Users
            { findById
            , findByLogin
            , has
            , store
            , remove
            }

findBy :: (?environment :: Environment, ToField p) => Query -> p -> MaybeT IO User
findBy q p = do
    result <- lift $ onlyQuery q p
    hoistMaybe $ case result of
        (user : _) -> fromUserRow user
        [] -> Nothing

data UserRow = UserRow
    { id :: ByteString
    , login :: Text
    , name :: Text
    , status :: Text
    }
    deriving stock (Generic, Eq, Show)
    deriving anyclass (FromRow, ToRow)

-- instance ToField UserRow where
--     toField user =
--         SQLBlob $ toStrict user.id

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

initUsers :: [Query]
initUsers =
    [ createUsersTable
    , createUsersIndex
    ]
  where
    createUsersTable =
        [sql|
                CREATE TABLE IF NOT EXISTS users 
                        (id BLOB PRIMARY KEY, login TEXT, name TEXT, status TEXT)
            |]

    createUsersIndex =
        [sql|
                CREATE INDEX IF NOT EXISTS users_login_idx 
                    ON users(login)
            |]
