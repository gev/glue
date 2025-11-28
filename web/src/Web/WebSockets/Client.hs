module Web.WebSockets.Client where

import Control.Exception (catch, throwIO)
import Network.WebSockets (HandshakeException, runClient)
import Web.WebSockets.Connection (WebSocketConnection, makeWebSocketConnection)
import Web.WebSockets.Error (WebSocketError (..))

type WebSocketClientApplication = WebSocketConnection -> IO ()

runWebSocketClient :: String -> Int -> String -> WebSocketClientApplication -> IO ()
runWebSocketClient host port path application =
  catch @HandshakeException
    do runClient host port path $ application . makeWebSocketConnection
    do throwIO . HandshakeError
