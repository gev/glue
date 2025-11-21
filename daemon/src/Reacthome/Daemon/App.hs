module Reacthome.Daemon.App where

import Data.ByteString (toStrict)
import Data.Foldable (for_)
import Data.Text (show)
import Data.Text.Encoding
import Data.UUID (UUID, toByteString)
import Reacthome.Relay.Client (RelayClient (..), makeRelayClient)
import Reacthome.Relay.Message (RelayMessage (..))
import Reacthome.Relay.Stat (RelayStat)
import Web.WebSockets.Client (WebSocketClientApplication)
import Prelude hiding (show)

application :: (?stat :: RelayStat) => UUID -> WebSocketClientApplication
application peer connection = do
    client <- makeRelayClient connection
    client.start
    let from = toStrict $ toByteString peer
    for_ [0 ..] \i ->
        client.send
            RelayMessage
                { peer = from
                , content = encodeUtf8 ("Hello Relay! " <> show @Int i)
                }
