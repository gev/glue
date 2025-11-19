module Web.WebSockets.Error where

import Control.Exception (Exception)
import Network.WebSockets (ConnectionException, HandshakeException)

data WebSocketError
    = SendError ConnectionException
    | ReceiveError ConnectionException
    | CloseError ConnectionException
    | HandshakeError HandshakeException
    | ConnectionError ConnectionException
    deriving (Show)

instance Exception WebSocketError
