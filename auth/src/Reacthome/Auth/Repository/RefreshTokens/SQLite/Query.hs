module Reacthome.Auth.Repository.RefreshTokens.SQLite.Query where

import Database.SQLite.Simple
import Database.SQLite.Simple.QQ

createRefreshTokensTable :: Query
createRefreshTokensTable =
    [sql|
        CREATE TABLE IF NOT EXISTS refresh_tokens 
            (token BLOB PRIMARY KEY, user_id BLOB)
    |]

findRefreshTokenByToken :: Query
findRefreshTokenByToken =
    [sql| 
        SELECT token, user_id
        FROM refresh_tokens WHERE token = ? 
    |]

storeRefreshToken :: Query
storeRefreshToken =
    [sql| 
        REPLACE INTO refresh_tokens (token, user_id)
        VALUES (?, ?)
    |]

removeRefreshToken :: Query
removeRefreshToken =
    [sql| 
        DELETE FROM refresh_tokens 
        WHERE token = ? 
    |]
