module Reacthome.Gate.Connection where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Text.Lazy (Text, empty)
import Data.UUID (UUID, toString)
import Network.HTTP.Types
import Network.WebSockets (Connection, HandshakeException, defaultConnectionOptions, receiveData, sendTextData)
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
    ExceptT String IO GateConnection
makeConnection uid onMessage onError = do
    message <- lift $ newMVar empty
    withExceptT show . except
        =<< lift do
            try @HandshakeException $
                void $
                    forkFinally
                        ( runSecureClientWith
                            ?environment.gate.host
                            ?environment.gate.port
                            ("/" <> toString uid)
                            defaultConnectionOptions
                            [(hSecWebSocketProtocol, ?environment.gate.protocol)]
                            (ws message onMessage onError)
                        )
                        (either onError pure)

    let send = putMVar message

    pure
        GateConnection
            { send
            }

ws ::
    MVar Text ->
    (Text -> IO ()) ->
    (SomeException -> IO ()) ->
    Connection ->
    IO ()
ws message onMessage onError connection = do
    void $
        forkFinally
            ( forever $
                onMessage =<< receiveData connection
            )
            (either onError id)

    let loop = do
            sendTextData connection =<< takeMVar message
            loop

    loop

hSecWebSocketProtocol :: HeaderName
hSecWebSocketProtocol = "Sec-WebSocket-Protocol"
