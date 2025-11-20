module Web.WebSockets.Client where

import Network.WebSockets (runClient)
import Web.WebSockets.Connection (WebSocketConnection, makeWebSocketConnection)

type WebSocketClientApplication = WebSocketConnection -> IO ()

runWebSocketClient :: String -> Int -> String -> WebSocketClientApplication -> IO ()
runWebSocketClient host port path application =
  runClient host port path $ application . makeWebSocketConnection
