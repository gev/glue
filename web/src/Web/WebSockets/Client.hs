module Web.WebSockets.Client where

import Control.Concurrent (threadDelay)
import Control.Exception (catch, throwIO)
import Control.Exception.Base (IOException)
import Control.Monad (forever)
import Network.WebSockets (HandshakeException, runClient)
import Web.WebSockets.Connection (WebSocketConnection, makeWebSocketConnection)
import Web.WebSockets.Error (WebSocketError (..))

type WebSocketClientApplication = WebSocketConnection -> IO ()

runWebSocketClient :: String -> Int -> String -> WebSocketClientApplication -> IO ()
runWebSocketClient host port path application =
    forever do
        catch @IOException
            do
                catch @HandshakeException
                    do runClient host port path $ application . makeWebSocketConnection
                    do throwIO . HandshakeError
            do print . IOException
        threadDelay 1_000_000
        print @String "Reconnect"
