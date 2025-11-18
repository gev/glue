module Reacthome.Daemon.App where

import Data.Foldable (traverse_)
import Data.Text (show)
import Data.Text.Encoding
import Data.UUID (UUID)
import Reacthome.Relay.Client (RelayClient (..), makeRelayClient)
import Reacthome.Relay.Message (RelayMessage (..))
import Web.WebSockets.Client (WebSocketClientApplication)
import Prelude hiding (show)

application :: UUID -> WebSocketClientApplication
application peer connection = do
    client.start
    traverse_ loop [0 ..]
  where
    client = makeRelayClient peer connection
    loop count = do
        client.send
            RelayMessage
                { peer
                , content = encodeUtf8 $ "Hello Relay! " <> show @Int count
                }
