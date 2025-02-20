module Reacthome.Gate.Connection.Pool where

import Control.Concurrent
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.HashMap.Strict
import Data.Text.Lazy (Text)
import Data.UUID
import Reacthome.Assist.Environment
import Reacthome.Gate.Connection
import Util.MVar
import Prelude hiding (lookup)

newtype GateConnectionPool = GateConnectionPool
    { getConnection :: UUID -> ExceptT String IO GateConnection
    }

makeConnectionPool ::
    ( ?environment :: Environment
    ) =>
    (Text -> IO ()) ->
    IO GateConnectionPool
makeConnectionPool onMessage = do
    pool <- newMVar empty

    let connect uid = do
            let onError _ = do
                    print $ "Disconnecting from " <> ?environment.gate.host <> ":" <> show ?environment.gate.port <> "/" <> toString uid
                    runModify pool $ delete uid

            connection <- makeConnection uid onMessage onError
            lift . runModify pool $ insert uid connection
            pure connection

    let getConnection uid =
            maybe (connect uid) pure
                =<< lift (runRead pool $ lookup uid)

    pure $
        GateConnectionPool
            { getConnection
            }
