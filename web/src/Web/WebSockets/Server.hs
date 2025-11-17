module Web.WebSockets.Server where

import Network.WebSockets (runServer)
import Web.WebSockets.PendingConnection (WebSocketPendingConnection, makeWebSocketPendingConnection)

type WebSocketServerApplication = WebSocketPendingConnection -> IO ()

runWebSocketServer :: String -> Int -> WebSocketServerApplication -> IO ()
runWebSocketServer host port application =
    runServer host port $ application . makeWebSocketPendingConnection
