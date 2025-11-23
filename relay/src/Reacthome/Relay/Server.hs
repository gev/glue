module Reacthome.Relay.Server where

import Control.Concurrent (yield)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.Chan.Unagi.NoBlocking (readChan)
import Control.Exception (catch)
import Control.Monad (forever, unless, when)
import Data.ByteString (toStrict)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.UUID (UUID, toByteString)
import GHC.Exts
import Reacthome.Relay.Error (RelayError (..), logError)
import Reacthome.Relay.Relay (Relay (..))
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

makeRelayServer :: Relay -> RelayServer
makeRelayServer relay = do
    let
        accept pending peer = do
            let from = toStrict $ toByteString peer
            catch @WebSocketError
                do
                    connection <- pending.accept
                    source <- relay.getSource from
                    concurrently_
                        do rxRun connection
                        do txRun connection source
                do logError . WebSocketError from

        rxRun connection = forever do
            raw <- connection.receiveMessage
            relay.sendMessage raw

        txRun connection source = do
            buffer <- newIORef []
            deadlineRef <- newIORef =<< getTime Monotonic
            let
                loop = do
                    now <- getTime Monotonic
                    deadline <- readIORef deadlineRef
                    when (toNanoSecs (diffTimeSpec now deadline) >= flushIntervalNs) do
                        flush
                        writeIORef deadlineRef =<< getTime Monotonic

                    msg <- readChan yield source

                    modifyIORef' buffer (msg :)
                    len <- length <$> readIORef buffer
                    when (len >= batchSize) do
                        flush
                        newDeadline <- getTime Monotonic
                        writeIORef deadlineRef newDeadline
                    loop

                flush = do
                    msgs <- reverse <$> readIORef buffer
                    unless (null msgs) do
                        writeIORef buffer []
                        connection.sendMessages msgs

            loop

    RelayServer{..}

batchSize :: Int
batchSize = 512

flushIntervalNs :: Integer
flushIntervalNs = 300_000
