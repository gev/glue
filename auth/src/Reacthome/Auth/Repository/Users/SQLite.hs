module Reacthome.Auth.Repository.Users.SQLite where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
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
        getAll = findAll pool getAllUsers

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
            { getAll
            , findById
            , findByLogin
            , has
            , store
            , remove
            }

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
    IO [User]
findAll pool q = do
    res <- runExceptT $ tryQuery_ pool q
    case res of
        Left e -> do
            print e
            pure []
        Right rows -> do
            let keys = fromUserRow <$> rows
                valid = filter (/= Nothing) keys
            pure $ fromJust <$> valid

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
