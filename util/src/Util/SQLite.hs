module Util.SQLite where

import Control.Exception
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Foldable
import Data.Pool
import Database.SQLite.Simple qualified as SQL

makePool :: String -> Double -> Int -> IO (Pool SQL.Connection)
makePool path ttl amount =
    newPool $
        defaultPoolConfig
            (openDB path)
            SQL.close
            ttl
            amount

openDB :: String -> IO SQL.Connection
openDB path = do
    connection <- SQL.open path
    traverse_
        (SQL.execute_ connection)
        [ "PRAGMA journal_mode=WAL"
        , "PRAGMA synchronous=NORMAL"
        , "PRAGMA wal_autocheckpoint=1000"
        ]
    pure connection

tryExecute ::
    (SQL.ToRow p) =>
    Pool SQL.Connection ->
    SQL.Query ->
    p ->
    ExceptT String IO ()
tryExecute pool query params =
    with pool \connection ->
        SQL.execute connection query params

tryExecute_ ::
    Pool SQL.Connection ->
    SQL.Query ->
    ExceptT String IO ()
tryExecute_ pool query =
    with pool (`SQL.execute_` query)

tryQuery ::
    (SQL.FromRow r, SQL.ToRow p) =>
    Pool SQL.Connection ->
    SQL.Query ->
    p ->
    ExceptT String IO [r]
tryQuery pool query params =
    with pool \connection ->
        SQL.query connection query params

tryQuery_ ::
    (SQL.FromRow r) =>
    Pool SQL.Connection ->
    SQL.Query ->
    ExceptT String IO [r]
tryQuery_ pool query =
    with pool (`SQL.query_` query)

with :: Pool SQL.Connection -> (SQL.Connection -> IO r) -> ExceptT String IO r
with pool action = do
    (connection, pool') <- lift $ takeResource pool
    either
        ( \e -> do
            lift $ destroyResource pool pool' connection
            throwE $ show e
        )
        ( \r -> do
            lift $ putResource pool' connection
            pure r
        )
        =<< lift (try @SomeException $ action connection)
