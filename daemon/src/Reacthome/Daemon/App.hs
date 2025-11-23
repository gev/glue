module Reacthome.Daemon.App where

import Control.Concurrent.Async (concurrently_)
import Control.Monad (forever, void)
import Data.ByteString (toStrict)
import Data.Text.Encoding
import Data.UUID (UUID, toByteString)
import Reacthome.Relay.Message (RelayMessage (..), serializeMessage)
import Reacthome.Relay.Stat (RelayHits (..), RelayStat (..))
import Web.WebSockets.Client (WebSocketClientApplication)
import Web.WebSockets.Connection (WebSocketConnection (..))
import Prelude hiding (show)

messagesPerChunk :: Int
messagesPerChunk = 512

application :: (?stat :: RelayStat) => UUID -> WebSocketClientApplication
application peer connection = do
    let from = toStrict $ toByteString peer
        chunk =
            replicate messagesPerChunk $
                serializeMessage
                    RelayMessage
                        { to = from
                        , from = from
                        , content = encodeUtf8 "Hello Reacthome Relay ;)"
                        }
    concurrently_
        do
            forever do
                connection.sendMessages chunk
                ?stat.tx.hit messagesPerChunk
        do
            forever do
                void connection.receiveMessage
                ?stat.rx.hit 1
