module WebSockets.Error where

import Control.Exception (Exception)
import GHC.IO.Exception (IOException)
import Network.WebSockets (ConnectionException, HandshakeException)

data WebSocketError
    = SendError ConnectionException
    | ReceiveError ConnectionException
    | CloseError ConnectionException
    | HandshakeError HandshakeException
    | IOError IOException
    deriving (Show)

instance Exception WebSocketError
