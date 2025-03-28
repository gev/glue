module Reacthome.Auth.Repository.Sqlite.PublicKeys where

import Database.SQLite.Simple
import Database.SQLite.Simple.QQ

initPublicKeys :: [Query]
initPublicKeys =
    [ createUsersTable
    , createUsersIndex
    , createPublicKeysTable
    , createPublicKeysIndex
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

    createPublicKeysTable =
        [sql| 
                CREATE TABLE IF NOT EXISTS public_keys 
                    (id BLOB PRIMARY KEY, user_id BLOB, alg INTEGER, bytes BLOB)
            |]

    createPublicKeysIndex =
        [sql|
                CREATE INDEX IF NOT EXISTS public_keys_uid_idx 
                    ON public_keys(user_id)
            |]
