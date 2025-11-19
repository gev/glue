module Reacthome.Daemon.App where

import Data.Foldable (traverse_)
import Data.Text.Lazy (show)
import Data.Text.Lazy.Encoding
import Data.UUID (UUID, toByteString)
import Reacthome.Relay.Client (RelayClient (..), makeRelayClient)
import Web.WebSockets.Client (WebSocketClientApplication)
import Prelude hiding (show)

application :: UUID -> WebSocketClientApplication
application peer connection = do
    client . start
    traverse_ loop [0 ..]
  where
    from = toByteString peer
    client = makeRelayClient connection
    loop count = do
        client . send $
            from <> encodeUtf8 ("Hello Relay! " <> show @Int count)
