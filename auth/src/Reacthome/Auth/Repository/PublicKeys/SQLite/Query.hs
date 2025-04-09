module Reacthome.Auth.Repository.PublicKeys.SQLite.Query where

import Database.SQLite.Simple
import Database.SQLite.Simple.QQ

createPublicKeysTable :: Query
createPublicKeysTable =
    [sql|
        CREATE TABLE IF NOT EXISTS public_keys 
            (id BLOB PRIMARY KEY, timestamp INTEGER, bytes BLOB)
    |]

createPublicKeysIndex :: Query
createPublicKeysIndex =
    [sql|
        CREATE INDEX IF NOT EXISTS public_keys_timestamp_idx 
            ON public_keys(timestamp)
    |]

getAllPublicKeys :: Query
getAllPublicKeys =
    [sql| 
        SELECT id, timestamp, bytes 
        FROM public_keys 
    |]

storePublicKey :: Query
storePublicKey =
    [sql| 
        REPLACE INTO public_keys (id, timestamp, bytes)
        VALUES (?, ?, ?)
    |]

cleanUpPublicKeys :: Query
cleanUpPublicKeys =
    [sql| 
        DELETE FROM public_keys 
        WHERE timestamp < ? 
    |]
