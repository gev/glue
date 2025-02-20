module Reacthome.Gate.Connection (
    GateConnection (..),
    makeConnection,
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Text.Lazy
import Data.UUID
import Network.HTTP.Types
import Network.WebSockets
import Reacthome.Assist.Environment
import Wuss

newtype GateConnection = GateConnection
    { send :: Text -> IO ()
    }

makeConnection ::
    ( ?environment :: Environment
    ) =>
    UUID ->
    (Text -> IO ()) ->
    (SomeException -> IO ()) ->
    IO GateConnection
makeConnection uid onMessage onError = do
    queue <- newTQueueIO
    void $
        forkFinally
            ( connect uid $
                run queue onMessage
            )
            (either onError pure)
    pure
        GateConnection
            { send = atomically . writeTQueue queue
            }

run ::
    TQueue Text ->
    (Text -> IO ()) ->
    Connection ->
    IO ()
run queue onMessage connection = do
    concurrently_
        (forever $ onMessage =<< receiveData connection)
        (forever $ sendTextData connection =<< atomically (readTQueue queue))

connect ::
    (?environment :: Environment) =>
    UUID ->
    ClientApp a ->
    IO a
connect uid = do
    runSecureClientWith
        ?environment.gate.host
        ?environment.gate.port
        ("/" <> toString uid)
        defaultConnectionOptions
        [(hSecWebSocketProtocol, ?environment.gate.protocol)]

hSecWebSocketProtocol :: HeaderName
hSecWebSocketProtocol = "Sec-WebSocket-Protocol"
