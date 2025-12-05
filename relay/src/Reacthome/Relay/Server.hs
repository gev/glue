module Reacthome.Relay.Server where

import Control.Concurrent.Async (race_)
import Control.Exception (handle)
import Data.ByteString (toStrict)
import Data.UUID (UUID, toByteString)
import Reacthome.Relay.Dispatcher (RelayDispatcher (..), RelaySink (..), RelaySource (..))
import Reacthome.Relay.Error (RelayError (..), logError)
import Reacthome.Relay.Message (getMessageDestination, isMessageDestinationValid)
import WebSockets.Connection (WebSocketConnection (..))
import WebSockets.Error (WebSocketError)
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
                dispatch message = do
                    let destination = getMessageDestination message
                    if isMessageDestinationValid destination
                        then do
                            !found <- ?dispatcher.getSink destination
                            case found of
                                Just !sink -> sink.sendMessage message
                                Nothing -> logError $ NoPeersFound destination
                        else logError $ InvalidDestination destination

                from = toStrict $ toByteString peer

                onError = logError . WebSocketError from
                wrap = handle @WebSocketError onError

            wrap do
                !connection <- pending.accept
                !source <- ?dispatcher.getSource from
                -- print $ "Peer connected " <> show peer
                race_
                    do wrap $ connection.runReceiveMessageLoop dispatch
                    do wrap $ connection.runSendMessageLoop source.tryReceiveMessage
                ?dispatcher.freeSource from
     in
        RelayServer{..}
