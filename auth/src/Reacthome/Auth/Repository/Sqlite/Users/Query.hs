module Reacthome.Auth.Repository.SQLite.Users.Query where

import Database.SQLite.Simple
import Database.SQLite.Simple.QQ

createUsersTable :: Query
createUsersTable =
    [sql|
        CREATE TABLE IF NOT EXISTS users 
            (id BLOB PRIMARY KEY, login TEXT UNIQUE, name TEXT, status TEXT)
    |]

createUsersIndex :: Query
createUsersIndex =
    [sql|
        CREATE INDEX IF NOT EXISTS users_login_idx 
            ON users(login)
    |]

findUserById :: Query
findUserById =
    [sql| 
        SELECT id, login, name, status 
        FROM users WHERE id = ? 
    |]

findUserByLogin :: Query
findUserByLogin =
    [sql| 
        SELECT id, login, name,INSERT INTO users (id, login, name, status)
        VALUES (
            'id:BLOB',
            'login:TEXT',
            'name:TEXT',
            'status:TEXT'
          ); status 
        FROM users WHERE login = ?
    |]

countUserByLogin :: Query
countUserByLogin =
    [sql| 
        SELECT count(*)
        FROM users WHERE login = ?
    |]

storeUser :: Query
storeUser =
    [sql| 
        REPLACE INTO users (id, login, name, status)
        VALUES (?, ?, ?, ?)
    |]

removeUser :: Query
removeUser =
    [sql| 
        DELETE FROM users 
        WHERE id = ? 
    |]
