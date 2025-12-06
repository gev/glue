module WebSockets.PendingConnection where

import Control.Exception (try)
import Control.Exception.Base (IOException)
import Data.ByteString (ByteString)
import Data.CaseInsensitive
import Network.WebSockets (HandshakeException, RequestHead (..), pendingRequest)
import Network.WebSockets.Connection (PendingConnection, acceptRequestWith, defaultAcceptRequest)
import WebSockets.Connection (WebSocketConnection, makeWebSocketConnection)
import WebSockets.Error (WebSocketError (..), joinExceptions)
import WebSockets.Options (WebSocketOptions)

type Headers = [(CI ByteString, ByteString)]

data WebSocketPendingConnection = WebSocketPendingConnection
    { headers :: Headers
    , path :: ByteString
    , accept :: IO (Either WebSocketError WebSocketConnection)
    }

makeWebSocketPendingConnection ::
    (?options :: WebSocketOptions) =>
    PendingConnection -> WebSocketPendingConnection
makeWebSocketPendingConnection pending =
    WebSocketPendingConnection
        { headers = request.requestHeaders
        , path = request.requestPath
        , accept
        }
  where
    request = pendingRequest pending
    accept =
        joinExceptions HandshakeError
            <$> try @IOException do
                try @HandshakeException do
                    connection <- acceptRequestWith pending defaultAcceptRequest
                    pure $ makeWebSocketConnection connection
