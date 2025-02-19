module Reacthome.Gate.Connection.Pool where

import Control.Concurrent
import Data.HashMap.Strict
import Data.UUID
import Network.WebSockets
import Reacthome.Assist.Environment
import Reacthome.Gate.Connection
import Util.MVar
import Prelude hiding (lookup)

newtype GateConnectionPool a = GateConnectionPool
    { getConnection :: UUID -> (a -> IO ()) -> IO GateConnection
    }

makeConnectionPool ::
    ( ?environment :: Environment
    , WebSocketsData a
    ) =>
    IO (GateConnectionPool a)
makeConnectionPool = do
    pool <- newMVar empty

    let connect uid onMessage = do
            let onClose = runModify pool $ delete uid
            connection <- makeConnection uid onMessage onClose
            runModify pool $ insert uid connection
            pure connection

    let getConnection uid onMessage =
            maybe (connect uid onMessage) pure
                =<< runRead pool (lookup uid)

    pure $
        GateConnectionPool
            { getConnection
            }
