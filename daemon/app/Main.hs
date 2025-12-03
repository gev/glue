import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently_, race_)
import Control.Exception (catch)
import Control.Monad (forever, replicateM, void)
import Data.ByteString (toStrict)
import Data.Foldable (for_)
import Data.IORef (newIORef, readIORef)
import Data.Text (unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Format.Numbers (prettyI)
import Data.UUID (toByteString)
import Data.UUID.V4 (nextRandom)
import GHC.IORef (atomicModifyIORef'_)
import Reacthome.Daemon.App (application)
import Reacthome.Relay.Message (RelayMessage (..), serializeMessage)
import Reacthome.Relay.Stat (RelayHits (hit, hits), RelayStat (rx, tx), makeRelayStat)
import System.Clock (Clock (..), diffTimeSpec, getTime, toNanoSecs)
import Web.WebSockets.Client (runWebSocketClient)
import Web.WebSockets.Connection (WebSocketConnection (sendMessages))
import Web.WebSockets.Error (WebSocketError)
import Prelude hiding (last)

concurrency :: Int
concurrency = 10_000

messagesPerChunk :: Int
messagesPerChunk = 30

main :: IO ()
main = do
    peers <- replicateM concurrency nextRandom
    stats <- replicateM concurrency makeRelayStat
    connections <- newIORef []
    let
        port = 3003
        host = "172.16.1.1"

        run (peer, stat, delay) = do
            threadDelay delay
            catch @WebSocketError
                do
                    let path = "/" <> show peer
                    let ?stat = stat
                    let ?register =
                            \connection ->
                                void $ atomicModifyIORef'_ connections (connection :)
                    let ?onMessage = const $ stat.rx.hit 1
                    let ?onError = print
                    runWebSocketClient host port path application
                print

        summarize x = sum <$> traverse (hits . x) stats

        summarizeStat = do
            r <- summarize rx
            t <- summarize tx
            pure (r, t)

        rps x1 x0 dt = fmt x1 <> " " <> " | RPS: " <> fmt ((x1 - x0) `div` dt)

        fmt = unpack . prettyI (Just '.')

        showStat = forever do
            t0 <- getTime Monotonic
            (rx0, tx0) <- summarizeStat
            threadDelay 1_000_000
            t1 <- getTime Monotonic
            (rx1, tx1) <- summarizeStat
            let !dt = fromInteger $ toNanoSecs (diffTimeSpec t1 t0) `div` 1_000_000_000
            n <- length <$> readIORef connections
            putStrLn $ "<-> " <> fmt n <> " connections"
            putStrLn $ "Tx: " <> rps tx1 tx0 dt
            putStrLn $ "Rx: " <> rps rx1 rx0 dt

        messagesPool = flip map (toStrict . toByteString <$> peers) \peer ->
            replicate messagesPerChunk $
                serializeMessage
                    RelayMessage
                        { to = peer
                        , from = peer
                        , content = encodeUtf8 "Hello Reacthome Relay ;)"
                        }

    race_
        do
            race_
                do
                    mapConcurrently_ run $ zip3 peers stats [1_000, 2_000 ..]
                showStat
        do
            threadDelay 20_000_000
            forever do
                connections' <- readIORef connections
                for_
                    (zip3 connections' stats messagesPool)
                    \(connection, stat, messages) -> do
                        connection.sendMessages messages
                        stat.tx.hit messagesPerChunk
