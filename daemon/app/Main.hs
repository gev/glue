import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (mapConcurrently_, race_)
import Control.Exception (catch, throwIO)
import Control.Monad (forever, replicateM, void)
import Data.ByteString (toStrict)
import Data.Foldable (for_, traverse_)
import Data.HashMap.Strict (delete, elems, empty, insert)
import Data.IORef (newIORef, readIORef)
import Data.Text (unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Format.Numbers (prettyI)
import Data.UUID (toByteString)
import Data.UUID.V4 (nextRandom)
import GHC.IORef (atomicModifyIORef'_)
import Reacthome.Daemon.App (application)
import Reacthome.Relay.Error (RelayError (WebSocketError))
import Reacthome.Relay.Message (RelayMessage (..), serializeMessage)
import Reacthome.Relay.Stat (RelayHits (hit, hits), RelayStat (rx, tx), makeRelayStat)
import System.Clock (Clock (..), diffTimeSpec, getTime, toNanoSecs)
import Web.WebSockets.Client (runWebSocketClient)
import Web.WebSockets.Connection (WebSocketConnection (..))
import Web.WebSockets.Error (WebSocketError)
import Prelude hiding (last)

concurrency :: Int
concurrency = 1

messagesPerChunk :: Int
messagesPerChunk = 1

main :: IO ()
main = do
    t0 <- getTime Monotonic
    let (rx0, tx0) = (0, 0)
    peers <- replicateM concurrency nextRandom
    stats <- replicateM concurrency makeRelayStat
    connections <- newIORef empty
    let
        port = 3003
        host = "172.16.1.1"

        run (peer, stat, delay) = forkIO do
            threadDelay delay
            let uid = toStrict $ toByteString peer
            let path = "/" <> show peer
            let ?stat = stat
            let ?register =
                    \connection ->
                        void . atomicModifyIORef'_ connections $
                            insert uid (uid, connection, stat)
            let ?onMessage = const $ stat.rx.hit 1
            let ?onError =
                    \e -> do
                        void . atomicModifyIORef'_ connections $ delete uid
                        print e
            runWebSocketClient host port path application

        summarize x = sum <$> traverse (hits . x) stats

        summarizeStat = do
            r <- summarize rx
            t <- summarize tx
            pure (r, t)

        rps x1 x0 dt = fmt x1 <> " " <> " | RPS: " <> fmt ((x1 - x0) `div` dt)

        fmt = unpack . prettyI (Just '.')

        showStat = forever do
            -- t0 <- getTime Monotonic
            -- (rx0, tx0) <- summarizeStat
            threadDelay 1_000_000
            t1 <- getTime Monotonic
            (rx1, tx1) <- summarizeStat
            let !dt = fromInteger $ toNanoSecs (diffTimeSpec t1 t0) `div` 1_000
            n <- length <$> readIORef connections
            putStrLn $ "<-> " <> fmt n <> " connections"
            putStrLn $ "Tx: " <> rps tx1 tx0 dt
            putStrLn $ "Rx: " <> rps rx1 rx0 dt

        doWork = forever do
            connections' <- elems <$> readIORef connections
            for_
                connections'
                \(uid, connection, stat) -> do
                    let messages =
                            replicate messagesPerChunk $
                                serializeMessage
                                    RelayMessage
                                        { to = uid
                                        , from = uid
                                        , content = encodeUtf8 "Hello Reacthome Relay ;)"
                                        }
                    catch @WebSocketError
                        do
                            connection.sendMessages messages
                            stat.tx.hit messagesPerChunk
                        \e -> do
                            print e
                            void . atomicModifyIORef'_ connections $ delete uid

    traverse_ run (zip3 peers stats [1000, 2000 ..])
    void $ forkIO doWork
    forever showStat
