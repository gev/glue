module Web.WebSockets.Server where

import Network.WebSockets (
    ConnectionOptions (connectionMessageDataSizeLimit),
    ServerOptions (..),
    SizeLimit (..),
    connectionFramePayloadSizeLimit,
    defaultConnectionOptions,
    runServerWithOptions,
 )
import Web.WebSockets.PendingConnection (WebSocketPendingConnection, makeWebSocketPendingConnection)

type WebSocketServerApplication = WebSocketPendingConnection -> IO ()

runWebSocketServer :: String -> Int -> WebSocketServerApplication -> IO ()
runWebSocketServer host port application =
    runServerWithOptions options $ application . makeWebSocketPendingConnection
  where
    options =
        ServerOptions
            { serverHost = host
            , serverPort = port
            , serverConnectionOptions =
                defaultConnectionOptions
                    { connectionFramePayloadSizeLimit = SizeLimit 256
                    , connectionMessageDataSizeLimit = SizeLimit 512
                    }
            }
