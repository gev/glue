module Web.WebSockets.Connection where

import Control.Exception (catch, throwIO)
import Data.ByteString (ByteString)
import Data.CaseInsensitive
import Network.WebSockets (
    Connection,
    HandshakeException,
    PendingConnection,
    RequestHead (..),
    acceptRequestWith,
    defaultAcceptRequest,
    pendingRequest,
 )
import Web.WebSockets.Error (WebSocketError (HandshakeError))

type Headers = [(CI ByteString, ByteString)]

data WebSocketPendingConnection = WebSocketPendingConnection
    { headers :: Headers
    , path :: ByteString
    , accept :: IO Connection
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
    accept = catch @HandshakeException
        do acceptRequestWith pending defaultAcceptRequest
        do throwIO . HandshakeError
