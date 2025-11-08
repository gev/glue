module Reacthome.Relay.App where

import Control.Monad (when)
import Data.Text (length, tail)
import Data.Text.Encoding (decodeUtf8)
import Data.UUID (fromText)
import Network.WebSockets (
    ServerApp,
    pendingRequest,
    requestPath,
 )
import Reacthome.Relay.Server (connect, makeRelayServer)
import Prelude hiding (length, splitAt, tail)

application :: ServerApp
application pending = do
    server <- makeRelayServer
    let path = decodeUtf8 pending.pendingRequest.requestPath
    when (length path > 1) do
        maybe
            (pure ())
            (server.connect pending)
            (fromText $ tail path)
