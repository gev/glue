module Web.WebSockets.Error where

import Control.Exception (Exception)
import Network.WebSockets (ConnectionException, HandshakeException)
import Prelude hiding (error)

data WebSocketError
    = SendError ConnectionException
    | ReceiveError ConnectionException
    | CloseError ConnectionException
    | HandshakeError HandshakeException
    | ConnectionError ConnectionException
    deriving (Show)

instance Exception WebSocketError
