module Reacthome.Relay.Server where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM (TBQueue, atomically, flushTBQueue, newTBQueueIO, writeTBQueue)
import Control.Exception (catch)
import Control.Monad (forever)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.UUID (UUID, toByteString)
import Reacthome.Relay.Error (RelayError (..), logError)
import Web.WebSockets.Connection (WebSocketConnection (..))
import Web.WebSockets.Error (WebSocketError)
import Web.WebSockets.PendingConnection (WebSocketPendingConnection (..))
import Prelude hiding (length, splitAt, tail, take)

newtype RelayServer = RelayServer
    { accept :: WebSocketPendingConnection -> UUID -> IO ()
    }

makeRelayServer :: IO RelayServer
makeRelayServer = do
    let
        accept pending peer = do
            let from = toStrict $ toByteString peer
            catch @WebSocketError
                do
                    connection <- pending.accept
                    queue <- newTBQueueIO 1000
                    concurrently_
                        do rxRun queue connection
                        do txRun queue connection
                do logError . WebSocketError from

    pure RelayServer{..}

rxRun :: TBQueue ByteString -> WebSocketConnection -> IO ()
rxRun queue connection = forever do
    message <- connection.receiveMessage
    atomically $ writeTBQueue queue message

txRun :: TBQueue ByteString -> WebSocketConnection -> IO ()
txRun queue connection = forever do
    messages <- atomically $ flushTBQueue queue
    connection.sendMessages messages
    threadDelay 1000

headerLength :: Int
headerLength = 16
