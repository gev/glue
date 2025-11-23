module Reacthome.Relay.Server where

import Control.Concurrent (yield)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.Chan.Unagi.NoBlocking (readChan)
import Control.Exception (catch)
import Control.Monad (forever)
import Data.ByteString (toStrict)
import Data.UUID (UUID, toByteString)
import Reacthome.Relay.Error (RelayError (..), logError)
import Reacthome.Relay.Relay (Relay (..), Source)
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

        txRun :: WebSocketConnection -> Source -> IO ()
        txRun connection source = forever do
            messages <- receive [] (400 :: Int)
            connection.sendMessages messages
          where
            receive ms 0 = pure $ reverse ms
            receive ms n = do
                m <- readChan yield source
                receive (m : ms) (n - 1)

    RelayServer{..}
