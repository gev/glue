module Web.WebSockets.Error where

import Control.Exception (Exception, IOException)
import Network.WebSockets (ConnectionException, HandshakeException)

data WebSocketError
    = SendError ConnectionException
    | ReceiveError ConnectionException
    | CloseError ConnectionException
    | ConnectionError WebSocketError
    | HandshakeError HandshakeException
    | IOError IOException
    deriving (Show)

instance Exception WebSocketError
