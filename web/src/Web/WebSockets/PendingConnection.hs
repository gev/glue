module Web.WebSockets.PendingConnection where

import Control.Exception (catch, throwIO)
import Data.ByteString (ByteString)
import Data.CaseInsensitive
import Network.WebSockets (
    HandshakeException,
    PendingConnection,
    RequestHead (..),
    acceptRequestWith,
    defaultAcceptRequest,
    pendingRequest,
 )
import Web.WebSockets.Connection (WebSocketConnection, makeWebSocketConnection)
import Web.WebSockets.Error (WebSocketError (..))

type Headers = [(CI ByteString, ByteString)]

data WebSocketPendingConnection = WebSocketPendingConnection
    { headers :: Headers
    , path :: ByteString
    , accept :: IO WebSocketConnection
    }

makeWebSocketPendingConnection :: PendingConnection -> WebSocketPendingConnection
makeWebSocketPendingConnection pending =
    WebSocketPendingConnection
        { headers = request.requestHeaders
        , path = request.requestPath
        , accept
        }
  where
    request = pendingRequest pending
    accept =
        catch @HandshakeException
            do
                connection <- acceptRequestWith pending defaultAcceptRequest
                pure $ makeWebSocketConnection connection
            do throwIO . HandshakeError
