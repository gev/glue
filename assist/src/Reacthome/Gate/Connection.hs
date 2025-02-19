module Reacthome.Gate.Connection where

import Control.Concurrent
import Control.Monad
import Data.Text (Text)
import Data.UUID (UUID, toString)
import Network.HTTP.Types
import Network.WebSockets (Connection, WebSocketsData, defaultConnectionOptions, receiveData, sendTextData)
import Reacthome.Assist.Environment
import Wuss

newtype GateConnection = GateConnection
    { send :: Text -> IO ()
    }

makeConnection ::
    ( ?environment :: Environment
    , WebSocketsData a
    ) =>
    UUID ->
    (a -> IO ()) ->
    IO () ->
    IO GateConnection
makeConnection uid onMessage onClose = do
    client <-
        runSecureClientWith
            ?environment.gate.host
            ?environment.gate.port
            (toString uid)
            defaultConnectionOptions
            [(hSecWebSocketProtocol, ?environment.gate.protocol)]
            $ ws onMessage onClose
    let send = sendTextData client
    pure
        GateConnection
            { send
            }

ws ::
    (WebSocketsData a) =>
    (a -> IO ()) ->
    IO () ->
    Connection ->
    IO Connection
ws onMessage onClose connection = do
    void $
        forkFinally
            ( forever $ do
                message <- receiveData connection
                onMessage message
            )
            (const onClose)

    pure connection

hSecWebSocketProtocol :: HeaderName
hSecWebSocketProtocol = "Sec-WebSocket-Protocol"
