module Reacthome.Gate.Connection.Pool where

import Control.Concurrent
import Data.HashMap.Strict
import Data.Text (Text)
import Data.UUID
import Reacthome.Assist.Environment
import Reacthome.Gate.Connection
import Util.MVar
import Prelude hiding (lookup)

newtype GateConnectionPool = GateConnectionPool
    { getConnection :: UUID -> IO GateConnection
    }

makeConnectionPool ::
    ( ?environment :: Environment
    ) =>
    (Text -> IO ()) ->
    IO GateConnectionPool
makeConnectionPool onMessage = do
    pool <- newMVar empty

    let connect uid = do
            let onClose = runModify pool $ delete uid
            connection <- makeConnection uid onMessage onClose
            runModify pool $ insert uid connection
            pure connection

    let getConnection uid =
            maybe (connect uid) pure
                =<< runRead pool (lookup uid)

    pure $
        GateConnectionPool
            { getConnection
            }
