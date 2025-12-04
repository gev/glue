import Control.Concurrent (forkIO, threadDelay, yield)
import Control.Monad (filterM, forever, replicateM, void, when)
import Data.ByteString (toStrict)
import Data.Foldable (for_, traverse_)
import Data.HashMap.Strict (elems, empty, insert)
import Data.IORef (newIORef, readIORef)
import Data.Text (unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Format.Numbers (prettyI)
import Data.UUID (toByteString)
import Data.UUID.V4 (nextRandom)
import GHC.IORef (atomicModifyIORef'_)
import Reacthome.Relay.Message (RelayMessage (..), serializeMessage)
import Reacthome.Relay.Stat (RelayHits (hit, hits), RelayStat (rx, tx), makeRelayStat)
import System.Clock (Clock (..), diffTimeSpec, getTime, toNanoSecs)
import Web.WebSockets.Client (WebSocketClient (..), runWebSocketClient)
import Web.WebSockets.Options (defaultWebSocketOptionsOptions)
import Prelude hiding (last)

concurrency :: Int
concurrency = 10_000

main :: IO ()
main = do
    peers <- replicateM concurrency nextRandom
    stats <- replicateM concurrency makeRelayStat
    clients <- newIORef empty
    let ?options = defaultWebSocketOptionsOptions
    let
        port = 3003
        host = "172.16.1.1"

        run (peer, stat, delay) = forkIO do
            threadDelay delay
            let uid = toStrict $ toByteString peer
            let path = "/" <> show peer
            let ?stat = stat
            client <- runWebSocketClient host port path
            void $ atomicModifyIORef'_ clients $ insert uid (uid, client, stat)
            void . forkIO $ forever do
                void client.receiveMessage
                stat.rx.hit 1

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
            (_, c, _) <- unzip3 . elems <$> readIORef clients
            n <- length <$> filterM isConnected c
            putStrLn $ "<-> " <> fmt n <> " connections"
            when (dt > 0) do
                putStrLn $ "Tx: " <> rps tx1 tx0 dt
                putStrLn $ "Rx: " <> rps rx1 rx0 dt

        doWork = do
            threadDelay 20_000_000
            forever do
                clients' <- elems <$> readIORef clients
                for_
                    clients'
                    \(uid, client, stat) -> do
                        let message =
                                serializeMessage
                                    RelayMessage
                                        { to = uid
                                        , from = uid
                                        , content = encodeUtf8 "Hello Reacthome Relay ;)"
                                        }
                        client.sendMessage message
                        stat.tx.hit 1
                yield

    traverse_ run (zip3 peers stats [1000, 2000 ..])
    void $ forkIO doWork
    forever showStat
