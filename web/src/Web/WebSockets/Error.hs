module Web.WebSockets.Error where

import Control.Exception (Exception, IOException)
import Network.WebSockets (ConnectionException, HandshakeException)

data WebSocketError
    = SendError ConnectionException
    | ReceiveError ConnectionException
    | CloseError ConnectionException
    | ConnectionError ConnectionException
    | HandshakeError HandshakeException
    | IOException IOException
    deriving (Show)

instance Exception WebSocketError
