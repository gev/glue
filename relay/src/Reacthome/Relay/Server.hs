module Reacthome.Relay.Server where

import Control.Concurrent (forkIO, yield)
import Control.Concurrent.Async (AsyncCancelled (AsyncCancelled), concurrently_, race_)
import Control.Concurrent.Chan.Unagi.NoBlocking (readChan)
import Control.Exception (SomeException, catch, finally, handle, throwIO)
import Control.Monad (forever, unless, void, when)
import Data.ByteString (toStrict)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.UUID (UUID, toByteString)
import GHC.Exts
import Reacthome.Relay.Dispatcher (RelayDispatcher (..))
import Reacthome.Relay.Error (RelayError (..), logError)
import System.Clock (diffTimeSpec, getTime, toNanoSecs)
import System.Clock.Seconds (Clock (..))
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
                from = toStrict $ toByteString peer

                onError = logError . WebSocketError from

                wrap = handle @WebSocketError onError

            source <- dispatcher.getSource from
            connection <- pending.accept
            print $ "Peer connected " <> show peer
            finally
                do
                    race_
                        do wrap $ runRx connection
                        do wrap $ runTx connection source
                do
                    print $ "Peer disconnected " <> show peer

        runRx connection = forever do
            dispatcher.sendMessage =<< connection.receiveMessage

        runTx connection source = do
            buffer <- newIORef []
            deadlineRef <- newIORef =<< getTime Monotonic
            let
                flush messages = do
                    writeIORef deadlineRef =<< getTime Monotonic
                    writeIORef buffer []
                    connection.sendMessages $ reverse messages

                checkDeadline now = do
                    deadline <- readIORef deadlineRef
                    let timeout = toNanoSecs (diffTimeSpec now deadline)
                    when (timeout >= flushIntervalNs) do
                        messages <- readIORef buffer
                        unless (null messages) do
                            flush messages

                enqueue message = do
                    modifyIORef' buffer (message :)
                    len <- length <$> readIORef buffer
                    when (len >= batchSize) do
                        flush =<< readIORef buffer

            forever do
                checkDeadline =<< getTime Monotonic
                enqueue =<< readChan yield source

    RelayServer{..}

batchSize :: Int
batchSize = 512

flushIntervalNs :: Integer
flushIntervalNs = 300_000
