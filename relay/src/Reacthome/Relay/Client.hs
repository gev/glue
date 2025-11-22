module Reacthome.Relay.Client where

-- import Control.Concurrent (forkIO, threadDelay)
-- import Control.Concurrent.Async (concurrently_)
-- import Control.Concurrent.STM (atomically, flushTBQueue, newTBQueueIO, writeTBQueue)
-- import Control.Monad (forever, void)
-- import Reacthome.Relay.Message (RelayMessage, serializeMessage)
-- import Reacthome.Relay.Stat (RelayHits (..), RelayStat (..))
-- import Web.WebSockets.Connection (WebSocketConnection (..))

-- newtype RelayClient = RelayClient
--     { send :: RelayMessage -> IO ()
--     }

-- makeRelayClient :: (?stat :: RelayStat) => WebSocketConnection -> IO RelayClient
-- makeRelayClient connection = do
--     queue <- newTBQueueIO 200
--     void . forkIO $ concurrently_
--         do
--             forever do
--                 void connection.receiveMessage
--                 ?stat.rx.hit 1
--         do
--             forever do
--                 messages <- atomically $ flushTBQueue queue
--                 connection.sendMessages messages
--                 ?stat.tx.hit $ length messages
--                 threadDelay 10_000

--     pure
--         RelayClient
--             { send = atomically . writeTBQueue queue . serializeMessage
--             }
