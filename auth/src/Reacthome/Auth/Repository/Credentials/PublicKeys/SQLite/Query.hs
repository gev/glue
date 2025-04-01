module Reacthome.Auth.Repository.Credentials.PublicKeys.SQLite.Query where

import Database.SQLite.Simple
import Database.SQLite.Simple.QQ

createPublicKeysTable :: Query
createPublicKeysTable =
    [sql|
        CREATE TABLE IF NOT EXISTS public_keys 
            (id BLOB PRIMARY KEY, user_id BLOB, algorithm TEXT, bytes BLOB)
    |]

createPublicKeysIndex :: Query
createPublicKeysIndex =
    [sql|
        CREATE INDEX IF NOT EXISTS public_keys_user_id_idx 
            ON public_keys(user_id)
    |]

findPublicKeyById :: Query
findPublicKeyById =
    [sql| 
        SELECT id, user_id, algorithm, bytes 
        FROM public_keys WHERE id = ? 
    |]

findPublicKeyByUserId :: Query
findPublicKeyByUserId =
    [sql| 
        SELECT id, user_id, algorithm, bytes 
        FROM public_keys WHERE user_id = ?
    |]

storePublicKey :: Query
storePublicKey =
    [sql| 
        REPLACE INTO public_keys (id, user_id, algorithm, bytes)
        VALUES (?, ?, ?, ?)
    |]

removePublicKey :: Query
removePublicKey =
    [sql| 
        DELETE FROM public_keys 
        WHERE id = ? 
    |]
