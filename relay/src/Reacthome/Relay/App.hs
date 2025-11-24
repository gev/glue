module Reacthome.Relay.App where

import Control.Monad (when)
import Data.Text (length, tail)
import Data.Text.Encoding (decodeUtf8)
import Data.UUID (fromText)
import Reacthome.Relay.Error (RelayError (..), logError)
import Reacthome.Relay.Server
import Web.WebSockets.PendingConnection (WebSocketPendingConnection (..))
import Web.WebSockets.Server (WebSocketServerApplication)
import Prelude hiding (length, splitAt, tail)

application :: RelayServer -> WebSocketServerApplication
application server pending = do
    let path = decodeUtf8 pending.path
    when (length path > 1) do
        let origin = tail path
        case fromText origin of
            Just peer -> server.accept pending peer
            Nothing -> logError $ InvalidUUID origin
