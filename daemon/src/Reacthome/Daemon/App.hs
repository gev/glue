module Reacthome.Daemon.App where

import Control.Concurrent (forkIO, threadDelay, yield)
import Control.Concurrent.Async (race_)
import Control.Exception (finally, handle)
import Control.Monad (forever, void)
import Data.ByteString (toStrict)
import Data.Text.Encoding
import Data.UUID (UUID, toByteString)
import Reacthome.Relay.Error (RelayError (..), logError)
import Reacthome.Relay.Message (RelayMessage (..), serializeMessage)
import Reacthome.Relay.Stat (RelayHits (..), RelayStat (..))
import Web.WebSockets.Client (WebSocketClientApplication)
import Web.WebSockets.Connection (WebSocketConnection (..))
import Web.WebSockets.Error (WebSocketError)

messagesPerChunk :: Int
messagesPerChunk = 1

application :: (?stat :: RelayStat) => UUID -> WebSocketClientApplication
application peer connection = do
    let
        from = toStrict $ toByteString peer

        onError = logError . WebSocketError from

        wrap = handle @WebSocketError onError

        chunk =
            replicate messagesPerChunk $
                serializeMessage
                    RelayMessage
                        { to = from
                        , from = from
                        , content = encodeUtf8 "Hello Reacthome Relay ;)"
                        }

        runTx = forever do
            connection.sendMessages chunk
            ?stat.tx.hit messagesPerChunk
            yield

        runRx = forever do
            void connection.receiveMessage
            ?stat.rx.hit 1

    -- finally
    --     do
    --         race_
    void . forkIO $ wrap runTx
    wrap runRx

-- do
--     print $ "Peer " <> show peer <> " Disconnected"
