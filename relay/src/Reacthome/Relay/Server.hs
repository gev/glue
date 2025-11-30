module Reacthome.Relay.Server where

import Control.Concurrent.Async (race_)
import Control.Exception (handle)
import Control.Monad (forever, void)
import Data.ByteString (toStrict)
import Data.UUID (UUID, toByteString)
import Data.Vector (toList, unsafeFreeze, unsafeTake)
import Data.Vector.Mutable (unsafeNew, unsafeWrite)
import Reacthome.Relay.Dispatcher (RelayDispatcher (..), RelaySink (..), RelaySource (..))
import Reacthome.Relay.Error (RelayError (..), logError)
import Reacthome.Relay.Message (getMessageDestination, isMessageDestinationValid)
import Web.WebSockets.Connection (WebSocketConnection (..))
import Web.WebSockets.Error (WebSocketError)
import Web.WebSockets.PendingConnection (WebSocketPendingConnection (..))
import Prelude hiding (lookup, take)

newtype RelayServer = RelayServer
    { accept :: WebSocketPendingConnection -> Peer -> IO ()
    }

type Peer = UUID

makeRelayServer :: (?dispatcher :: RelayDispatcher) => RelayServer
makeRelayServer =
    let
        accept pending peer = do
            let
                runRx connection = forever do
                    !message <- connection.receiveMessage
                    let destination = getMessageDestination message
                    if isMessageDestinationValid destination
                        then do
                            !found <- ?dispatcher.getSink destination
                            case found of
                                Just !sink -> sink.sendMessage message
                                Nothing -> logError $ NoPeersFound destination
                        else logError $ InvalidDestination destination

                runTx connection source = do
                    let
                        processMessageLoop !vector !index = do
                            (!maybeMessage, !waitMessage) <- source.tryReceiveMessage
                            case maybeMessage of
                                Nothing -> do
                                    !actualVector <-
                                        if index > 0
                                            then sendBatch vector index
                                            else pure vector
                                    !message <- waitMessage
                                    unsafeWrite actualVector 0 message
                                    processMessageLoop actualVector 1
                                Just !message -> do
                                    unsafeWrite vector index message
                                    let nextIndex = index + 1
                                    if nextIndex < batchSize
                                        then do
                                            processMessageLoop vector nextIndex
                                        else do
                                            newVector <- sendBatch vector batchSize
                                            processMessageLoop newVector 0

                        sendBatch !vector !size = do
                            !frozen <- unsafeFreeze vector
                            let !messages = toList $ unsafeTake size frozen
                            void $ connection.sendMessages messages
                            unsafeNew batchSize

                    initialVector <- unsafeNew batchSize
                    processMessageLoop initialVector 0

                from = toStrict $ toByteString peer

                onError = logError . WebSocketError from
                wrap = handle @WebSocketError onError

            wrap do
                !connection <- pending.accept
                -- print $ "Peer connected " <> show peer
                !source <- ?dispatcher.getSource from
                race_
                    do wrap $ runRx connection
                    do wrap $ runTx connection source
                ?dispatcher.freeSource from
     in
        RelayServer{..}

batchSize :: Int
batchSize = 40
