module Reacthome.Relay.Server where

import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM (TBQueue, atomically, newTBQueueIO, readTBQueue, writeTBQueue)
import Control.Exception (catch)
import Control.Monad (forever)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Int (Int64)
import Data.Time.Clock.System (SystemTime (..), getSystemTime)
import Data.UUID (UUID, toByteString)
import Reacthome.Relay.Error (RelayError (..), logError)
import Web.WebSockets.Connection (WebSocketConnection (..))
import Web.WebSockets.Error (WebSocketError)
import Web.WebSockets.PendingConnection (WebSocketPendingConnection (..))
import Prelude hiding (last, splitAt, tail, take)

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
                    queue <- newTBQueueIO 100
                    concurrently_
                        do rxRun queue connection
                        do txRun queue connection
                do logError . WebSocketError from

    pure RelayServer{..}

rxRun :: TBQueue [ByteString] -> WebSocketConnection -> IO ()
rxRun queue connection = do
    let go batch last = do
            msg <- connection.receiveMessage
            now <- getSystemTime
            if diffSystemTime now last >= 300_000 && not (null batch)
                then do
                    atomically $ writeTBQueue queue (reverse batch)
                    go [msg] now
                else
                    go (msg : batch) last
    start <- getSystemTime
    go [] start

txRun :: TBQueue [ByteString] -> WebSocketConnection -> IO ()
txRun queue connection = forever do
    messages <- atomically $ readTBQueue queue
    connection.sendMessages messages

headerLength :: Int
headerLength = 16

diffSystemTime :: SystemTime -> SystemTime -> Int64
diffSystemTime (MkSystemTime s1 ns1) (MkSystemTime s2 ns2) =
    (s1 - s2) * 1_000_000_000 + (fromIntegral ns1 - fromIntegral ns2)
