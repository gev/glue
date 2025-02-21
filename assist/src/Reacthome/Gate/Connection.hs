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
    queue <- newTBQueueIO $ fromIntegral ?environment.queueSize
    void $
        forkFinally
            ( connect uid $
                run queue onMessage
            )
            (either onError pure)
    pure
        GateConnection
            { send = atomically . writeTBQueue queue
            }

run ::
    TBQueue Text ->
    (Text -> IO ()) ->
    Connection ->
    IO ()
run queue onMessage connection =
    concurrently_
        (forever $ onMessage =<< receiveData connection)
        (forever $ sendTextData connection =<< atomically (readTBQueue queue))

connect ::
    (?environment :: Environment) =>
    UUID ->
    ClientApp a ->
    IO a
connect uid =
    runSecureClientWith
        ?environment.gate.host
        ?environment.gate.port
        ("/" <> toString uid)
        defaultConnectionOptions
        [(hSecWebSocketProtocol, ?environment.gate.protocol)]

hSecWebSocketProtocol :: HeaderName
hSecWebSocketProtocol = "Sec-WebSocket-Protocol"
