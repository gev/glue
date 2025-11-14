module Web.WebSockets.Client where

import Control.Monad (forever, (<=<))
import Data.ByteString.Lazy (ByteString)
import Network.WebSockets (defaultPingPongOptions, runServer, withPingPong)
import Web.WebSockets.Connection (WebSocketPendingConnection (accept), makeWebSocketPendingConnection)
import Web.WebSockets.Socket (WebSocket, makeWebSocket, receiveMessage)

type WebSocketServer = WebSocketPendingConnection -> IO ()

makeWebsocketClient :: WebSocketPendingConnection -> (ByteString -> IO ()) -> IO WebSocket
makeWebsocketClient pending handle = do
    connection <- pending.accept
    withPingPong defaultPingPongOptions connection run
    pure $ makeWebSocket connection
  where
    run = forever . loop
    loop = handle <=< receiveMessage

runWebSocketClient :: String -> Int -> WebSocketServer -> IO ()
runWebSocketClient host port application =
    runClient host port $ application . makeWebSocketPendingConnection
