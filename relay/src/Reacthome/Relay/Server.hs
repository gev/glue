{-# LANGUAGE Strict #-}

module Reacthome.Relay.Server where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (readTChan)
import Control.Exception (catch)
import Control.Monad (forever, void)
import Data.ByteString (toStrict)
import Data.UUID (UUID, toByteString)
import Reacthome.Relay.Error (RelayError (..), logError)
import Reacthome.Relay.Message (PeerMessage (..), RelayMessage (..), parseMessage)
import Reacthome.Relay.Relay (Relay (..))
import Web.WebSockets.Connection (WebSocketConnection (..))
import Web.WebSockets.Error (WebSocketError)
import Web.WebSockets.PendingConnection (WebSocketPendingConnection (..))
import Prelude hiding (last, lookup, splitAt, tail, take)

type RelayServer = WebSocketPendingConnection -> Peer -> IO ()
type Peer = UUID

makeRelayServer :: Relay -> RelayServer
makeRelayServer relay pending peer = do
    let
        from = toStrict $ toByteString peer

        rxRun connection = forever do
            raw <- connection.receiveMessage
            catch @RelayError
                do
                    let message = parseMessage raw
                    atomically do
                        relay.sendMessage
                            RelayMessage
                                { from
                                , to = message.peer
                                , content = message.content
                                }
                do
                    logError

        txRun connection source = forever do
            messages <- atomically $ receive [] (40 :: Int)
            connection.sendMessages messages
          where
            receive ms 0 = pure $ reverse ms
            receive ms n = do
                m <- readTChan source
                receive (m : ms) (n - 1)

    void $ forkIO relay.dispatch

    catch @WebSocketError
        do
            connection <- pending.accept
            source <- atomically $ relay.getSource from
            concurrently_
                do rxRun connection
                do txRun connection source
        do logError . WebSocketError from
