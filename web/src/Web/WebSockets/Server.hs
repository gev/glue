module Web.WebSockets.Server where

import Control.Monad (forever, (<=<))
import Data.ByteString.Lazy (ByteString)
import Network.WebSockets (defaultPingPongOptions, runServer, withPingPong)
import Web.WebSockets.Connection (WebSocketPendingConnection (accept), makeWebSocketPendingConnection)
import Web.WebSockets.Socket (WebSocket, makeWebSocket, receiveMessage)

type WebSocketServer = WebSocketPendingConnection -> IO ()

makeWebsocketServer :: WebSocketPendingConnection -> (ByteString -> IO ()) -> IO WebSocket
makeWebsocketServer pending handle = do
    connection <- pending.accept
    withPingPong defaultPingPongOptions connection run
    pure $ makeWebSocket connection
  where
    run = forever . loop
    loop = handle <=< receiveMessage

runWebSocketServer :: String -> Int -> WebSocketServer -> IO ()
runWebSocketServer host port application =
    runServer host port $ application . makeWebSocketPendingConnection
