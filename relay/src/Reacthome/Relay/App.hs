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
import Reacthome.Relays (connect, makeRelays)
import Prelude hiding (length, splitAt, tail)

application :: ServerApp
application pending = do
    relays <- makeRelays
    let path = decodeUtf8 pending.pendingRequest.requestPath
    when (length path > 1) do
        maybe
            (pure ())
            (relays.connect pending)
            (fromText $ tail path)
