module Reacthome.Relay.Server where

import Control.Concurrent (threadDelay, yield)
import Control.Concurrent.Async (race_)
import Control.Exception (finally, handle)
import Control.Monad (forever, unless)
import Data.ByteString (toStrict)
import Data.Foldable (traverse_)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List.Split (chunksOf)
import Data.Sequence (empty, (|>))
import Data.UUID (UUID, toByteString)
import GHC.Exts
import GHC.IORef (atomicModifyIORef'_)
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

makeRelayServer :: RelayDispatcher -> RelayServer
makeRelayServer dispatcher = do
    let
        accept pending peer = do
            let
                runRx connection = wrap $ forever do
                    !message <- connection.receiveMessage
                    yield
                    connection.sendMessage message
                -- let destination = getMessageDestination message
                -- if isMessageDestinationValid destination
                --     then do
                --         !found <- dispatcher.getSink destination
                --         case found of
                --             Just !sink -> sink.sendMessage message
                --             Nothing -> logError $ NoPeersFound destination
                --     else logError $ InvalidDestination destination

                -- runTx connection source = do
                --     buffer <- newIORef empty
                --     let
                --         collectMessages = forever do
                --             !message <- source.receiveMessage
                --             atomicModifyIORef'_ buffer (|> message)

                --         transmitMessages = wrap $ forever do
                --             !messages <- atomicModifyIORef' buffer (empty,)
                --             unless (null messages) do
                --                 let !chunks = chunksOf batchSize $ toList messages
                --                 traverse_ connection.sendMessages chunks
                --             threadDelay flushIntervalUs

                --     race_
                --         collectMessages
                --         transmitMessages

                from = toStrict $ toByteString peer

                onError = logError . WebSocketError from
                wrap = handle @WebSocketError onError

            wrap do
                connection <- pending.accept
                runRx connection
    -- print $ "Peer connected " <> show peer
    -- source <- dispatcher.getSource from
    -- finally
    --     do
    --         race_
    --             do runRx connection
    --             do runTx connection source
    --     do
    --         -- print $ "Peer disconnected " <> show peer
    --         dispatcher.freeSource from

    RelayServer{..}

batchSize :: Int
batchSize = 40

flushIntervalUs :: Int
flushIntervalUs = 100
