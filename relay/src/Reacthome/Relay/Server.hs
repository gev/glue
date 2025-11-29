module Reacthome.Relay.Server where

import Control.Concurrent.Async (race_)
import Control.Exception (handle)
import Control.Monad (forever)
import Data.ByteString (toStrict)
import Data.UUID (UUID, toByteString)
import GHC.Exts
import Reacthome.Relay.Dispatcher (RelayDispatcher (..), RelaySink (..), RelaySource (..))
import Reacthome.Relay.Error (RelayError (..), logError)
import Reacthome.Relay.Message (getMessageDestination, isMessageDestinationValid)
import Web.WebSockets.Connection (WebSocketConnection (..))
import Web.WebSockets.Error (WebSocketError)
import Web.WebSockets.PendingConnection (WebSocketPendingConnection (..))
import Prelude hiding (last, lookup, splitAt, tail, take)

newtype RelayServer = RelayServer
    { accept :: WebSocketPendingConnection -> Peer -> IO ()
    }

type Peer = UUID

makeRelayServer :: (?dispatcher :: RelayDispatcher) => RelayServer
makeRelayServer = do
    let
        accept pending peer = do
            let
                runRx connection = wrap $ forever do
                    !message <- connection.receiveMessage
                    let destination = getMessageDestination message
                    if isMessageDestinationValid destination
                        then do
                            !found <- ?dispatcher.getSink destination
                            case found of
                                Just !sink -> sink.sendMessage message
                                Nothing -> logError $ NoPeersFound destination
                        else logError $ InvalidDestination destination

                runTx connection source = wrap $ forever do
                    !message <- source.receiveMessage
                    connection.sendMessage message

                from = toStrict $ toByteString peer

                onError = logError . WebSocketError from
                wrap = handle @WebSocketError onError

            wrap do
                !connection <- pending.accept
                -- print $ "Peer connected " <> show peer
                !source <- ?dispatcher.getSource from
                race_
                    do runRx connection
                    do runTx connection source
                ?dispatcher.freeSource from

    RelayServer{..}

batchSize :: Int
batchSize = 40

flushIntervalUs :: Int
flushIntervalUs = 300
