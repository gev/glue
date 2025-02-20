module Reacthome.Gate.Connection where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Text.Lazy (Text)
import Data.UUID (UUID, toString)
import Network.HTTP.Types
import Network.WebSockets (Connection, HandshakeException, WebSocketsData, defaultConnectionOptions, receiveData, sendClose, sendTextData)
import Reacthome.Assist.Environment
import Wuss

data GateConnection = GateConnection
    { send :: Text -> IO ()
    , close :: IO ()
    }

makeConnection ::
    ( ?environment :: Environment
    , WebSocketsData a
    ) =>
    UUID ->
    (a -> IO ()) ->
    IO () ->
    ExceptT String IO GateConnection
makeConnection uid onMessage onClose = do
    client <-
        withExceptT show . except
            =<< lift
                ( try @HandshakeException
                    $ runSecureClientWith
                        ?environment.gate.host
                        ?environment.gate.port
                        (toString uid)
                        defaultConnectionOptions
                        [(hSecWebSocketProtocol, ?environment.gate.protocol)]
                    $ ws onMessage onClose
                )
    let send = sendTextData client
    let close = sendClose @Text client "Close" >> onClose
    pure
        GateConnection
            { send
            , close
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
