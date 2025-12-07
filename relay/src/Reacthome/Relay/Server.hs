module Reacthome.Relay.Server where

import Control.Concurrent.Async (race)
import Data.ByteString (toStrict)
import Data.UUID (UUID, toByteString)
import Reacthome.Relay.Dispatcher (RelayDispatcher (..))
import Reacthome.Relay.Error (RelayError (..), logError)
import Reacthome.Relay.Message (getMessageDestination, isMessageDestinationValid)
import WebSockets.Connection (WebSocketConnection (..))
import WebSockets.Options (WebSocketOptions)
import WebSockets.PendingConnection (WebSocketPendingConnection (..))
import Prelude hiding (lookup, take)

newtype RelayServer = RelayServer
    { accept :: WebSocketPendingConnection -> Peer -> IO ()
    }

type Peer = UUID

makeRelayServer ::
    ( ?options :: WebSocketOptions
    , ?dispatcher :: RelayDispatcher
    ) =>
    RelayServer
makeRelayServer =
    let
        accept pending peer = do
            let
                dispatchMessage message = do
                    let destination = getMessageDestination message
                    if isMessageDestinationValid destination
                        then do
                            !found <- ?dispatcher.getSink destination
                            case found of
                                Just !sendMessage -> sendMessage message
                                Nothing -> logError $ NoPeersFound destination
                        else logError $ InvalidDestination destination

                from = toStrict $ toByteString peer

            !successful <- pending.accept
            case successful of
                Left !e -> logError $ WebSocketError from e
                Right !connection -> do
                    !tryReceiveMessage <- ?dispatcher.getSource from
                    -- print $ "Peer connected " <> show peer
                    res <-
                        either id id <$> race
                            do connection.runReceiveMessageLoop dispatchMessage
                            do connection.runSendMessageLoop tryReceiveMessage
                    logError $ WebSocketError from res
                    ?dispatcher.freeSource from
     in
        RelayServer{..}
