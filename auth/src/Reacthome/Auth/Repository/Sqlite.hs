module Reacthome.Auth.Repository.Sqlite where

import Control.Exception
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Reacthome.Auth.Environment

makeDB :: String -> IO Connection
makeDB = open

tryExecute ::
    (?environment :: Environment, ToRow p) =>
    Query ->
    p ->
    ExceptT String IO ()
tryExecute q p =
    withExceptT show . except
        =<< lift (try @SQLError $ execute ?environment.db q p)

onlyExecute ::
    (?environment :: Environment, ToField p) =>
    Query ->
    p ->
    IO ()
onlyExecute q = execute ?environment.db q . Only

onlyQuery ::
    (?environment :: Environment, ToField p, FromRow r) =>
    Query ->
    p ->
    IO [r]
onlyQuery q = query ?environment.db q . Only
