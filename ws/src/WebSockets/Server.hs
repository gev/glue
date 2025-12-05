module WebSockets.Server where

import Network.WebSockets (runServer)
import WebSockets.Options (WebSocketOptions)
import WebSockets.PendingConnection (WebSocketPendingConnection, makeWebSocketPendingConnection)

type WebSocketServerApplication = WebSocketPendingConnection -> IO ()

runWebSocketServer ::
    (?options :: WebSocketOptions) =>
    String -> Int -> WebSocketServerApplication -> IO ()
runWebSocketServer host port application =
    runServer host port $ application . makeWebSocketPendingConnection
