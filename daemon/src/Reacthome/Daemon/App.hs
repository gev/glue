module Reacthome.Daemon.App where

import Data.ByteString (toStrict)
import Data.Foldable (for_)
import Data.Text (show)
import Data.Text.Encoding
import Data.UUID (UUID, toByteString)
import Reacthome.Relay.Client (RelayClient (..), makeRelayClient)
import Reacthome.Relay.Message (RelayMessage (..))
import Reacthome.Relay.Stat (RelayHits (hit), RelayStat (tx))
import Web.WebSockets.Client (WebSocketClientApplication)
import Prelude hiding (show)

application :: UUID -> RelayStat -> WebSocketClientApplication
application peer stat connection = do
    client <- makeRelayClient stat connection
    client.start
    let from = toStrict $ toByteString peer
    for_ [0 ..] \i -> do
        client.send
            RelayMessage
                { peer = from
                , content = encodeUtf8 ("Hello Relay! " <> show @Int i)
                }
        stat.tx.hit
