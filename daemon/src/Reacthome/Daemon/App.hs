module Reacthome.Daemon.App where

import Data.ByteString (toStrict)
import Data.ByteString.Short (toShort)
import Data.Foldable (traverse_)
import Data.Text (show)
import Data.Text.Encoding
import Data.UUID (UUID, toByteString)
import Reacthome.Relay.Client (RelayClient (..), makeRelayClient)
import Web.WebSockets.Client (WebSocketClientApplication)
import Prelude hiding (show)

application :: UUID -> WebSocketClientApplication
application peer connection = do
    client.start
    traverse_ loop [0 ..]
  where
    from = toStrict $ toByteString peer
    client = makeRelayClient connection
    loop count = do
        client.send . toShort $
            from <> encodeUtf8 ("Hello Relay! " <> show @Int count)
