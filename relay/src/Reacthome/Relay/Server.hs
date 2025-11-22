module Reacthome.Relay.Server where

import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, dupTChan, newBroadcastTChan, readTChan, writeTChan)
import Control.Exception (catch)
import Control.Monad (forever)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.Time.Clock.System (SystemTime (..), getSystemTime)
import Data.UUID (UUID, toByteString)
import Reacthome.Relay.Error (RelayError (..), logError)
import StmContainers.Map (insert, lookup, newIO)
import Web.WebSockets.Connection (WebSocketConnection (..))
import Web.WebSockets.Error (WebSocketError)
import Web.WebSockets.PendingConnection (WebSocketPendingConnection (..))
import Prelude hiding (last, lookup, splitAt, tail, take)

newtype RelayServer = RelayServer
    { accept :: WebSocketPendingConnection -> UUID -> IO ()
    }

makeRelayServer :: IO RelayServer
makeRelayServer = do
    sinks <- newIO
    let
        accept pending peer = do
            let from = toStrict $ toByteString peer
            catch @WebSocketError
                do
                    (sink, source) <-
                        atomically do
                            maybe
                                do
                                    sink <- newBroadcastTChan
                                    insert sink from sinks
                                    source <- dupTChan sink
                                    pure (sink, source)
                                do
                                    \sink -> do
                                        source <- dupTChan sink
                                        pure (sink, source)
                                =<< lookup from sinks
                    connection <- pending.accept
                    concurrently_
                        do rxRun connection sink
                        do txRun connection source
                do logError . WebSocketError from

    pure RelayServer{..}

rxRun :: WebSocketConnection -> TChan ByteString -> IO ()
rxRun connection sink = do
    go [] =<< getSystemTime
  where
    go batch last = do
        msg <- connection.receiveMessage
        now <- getSystemTime
        if diffSystemTime now last >= 500_000
            then do
                atomically $ traverse_ (writeTChan sink) (reverse batch)
                go [msg] now
            else
                go (msg : batch) last

txRun :: WebSocketConnection -> TChan ByteString -> IO ()
txRun connection source = forever do
    messages <- atomically $ go [] (40 :: Int)
    connection.sendMessages $ reverse messages
  where
    go batch 0 = pure batch
    go batch n = do
        msg <- readTChan source
        go (msg : batch) (n - 1)

headerLength :: Int
headerLength = 16

diffSystemTime :: SystemTime -> SystemTime -> Int64
diffSystemTime (MkSystemTime s1 ns1) (MkSystemTime s2 ns2) =
    (s1 - s2) * 1_000_000_000 + (fromIntegral ns1 - fromIntegral ns2)
