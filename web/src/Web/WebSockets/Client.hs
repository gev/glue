module Web.WebSockets.Client where

import Network.WebSockets (
  ConnectionOptions (..),
  SizeLimit (..),
  defaultConnectionOptions,
  runClientWith,
 )
import Web.WebSockets.Connection (WebSocketConnection, makeWebSocketConnection)

type WebSocketClientApplication = WebSocketConnection -> IO ()

runWebSocketClient :: String -> Int -> String -> WebSocketClientApplication -> IO ()
runWebSocketClient host port path application =
  runClientWith host port path options [] $ application . makeWebSocketConnection
 where
  options =
    defaultConnectionOptions
      { connectionFramePayloadSizeLimit = SizeLimit 256
      , connectionMessageDataSizeLimit = SizeLimit 512
      }
