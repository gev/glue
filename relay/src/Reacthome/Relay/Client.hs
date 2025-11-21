module Reacthome.Relay.Client where

import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import Reacthome.Relay.Message (RelayMessage, serializeMessage)
import Reacthome.Relay.Stat (RelayHits (..), RelayStat (..))
import Web.WebSockets.Connection (WebSocketConnection (..))
import Prelude hiding (length, splitAt, tail)

data RelayClient = RelayClient
    { start :: IO ()
    , send :: RelayMessage -> IO ()
    }

makeRelayClient :: (?stat :: RelayStat) => WebSocketConnection -> IO RelayClient
makeRelayClient connection = do
    let
        start =
            void $ forkIO $ forever do
                void connection.receiveMessage
                ?stat.rx.hit

        send message = do
            connection.sendMessage $ serializeMessage message
            ?stat.tx.hit

    pure RelayClient{..}
