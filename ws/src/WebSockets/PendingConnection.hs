module WebSockets.PendingConnection where

import Control.Exception (catch, throwIO)
import Control.Exception.Base (IOException)
import Data.ByteString (ByteString)
import Data.CaseInsensitive
import Network.WebSockets (HandshakeException, RequestHead (..), pendingRequest)
import Network.WebSockets.Connection (PendingConnection, acceptRequestWith, defaultAcceptRequest)
import WebSockets.Connection (WebSocketConnection, makeWebSocketConnection)
import WebSockets.Error (WebSocketError (..))
import WebSockets.Options (WebSocketOptions)

type Headers = [(CI ByteString, ByteString)]

data WebSocketPendingConnection = WebSocketPendingConnection
    { headers :: Headers
    , path :: ByteString
    , accept :: IO WebSocketConnection
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
        catch @IOException
            do
                catch @HandshakeException
                    do
                        connection <- acceptRequestWith pending defaultAcceptRequest
                        pure $ makeWebSocketConnection connection
                    do throwIO . HandshakeError
            do throwIO . IOError
