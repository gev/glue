import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan.Unagi.Bounded (Element (tryRead), newChan, tryReadChan, writeChan)
import Control.Monad (filterM, forever, replicateM, void, when)
import Data.ByteString (toStrict)
import Data.Foldable (for_, traverse_)
import Data.HashMap.Strict (elems, empty, insert)
import Data.IORef (newIORef, readIORef)
import Data.List (unzip4)
import Data.Text (unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Format.Numbers (prettyI)
import Data.UUID (toByteString)
import Data.UUID.V4 (nextRandom)
import GHC.IORef (atomicModifyIORef'_)
import Reacthome.Relay.Message (RelayMessage (..), serializeMessage)
import Reacthome.Relay.Stat (RelayHits (hit, hits), RelayStat (rx, tx), makeRelayStat)
import System.Clock (Clock (..), diffTimeSpec, getTime, toNanoSecs)
import WebSockets.Client (WebSocketClient (..), runWebSocketClient)
import WebSockets.Options (bound, defaultWebSocketOptions)
import Prelude hiding (last)

concurrency :: Int
concurrency = 100

main :: IO ()
main = do
    peers <- replicateM concurrency nextRandom
    stats <- replicateM concurrency makeRelayStat
    clients <- newIORef empty
    let ?options = defaultWebSocketOptions
    let
        port = 3003
        host = "172.16.1.1"

        run (peer, stat, delay) = forkIO do
            threadDelay delay
            let uid = toStrict $ toByteString peer
            let path = "/" <> show peer
            let ?stat = stat
            (inChan, outChan) <- newChan ?options.bound
            let ?sink = const $ stat.rx.hit 1
            let ?source =
                    do
                        (!element, !wait) <- tryReadChan outChan
                        !message <- tryRead element
                        pure (message, wait)
            client <- runWebSocketClient host port path
            void $ atomicModifyIORef'_ clients $ insert uid (uid, inChan, client, stat)

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
            (_, _, c, _) <- unzip4 . elems <$> readIORef clients
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
                    \(uid, inChan, _, stat) -> do
                        let message =
                                serializeMessage
                                    RelayMessage
                                        { to = uid
                                        , from = uid
                                        , content = encodeUtf8 "Hello Reacthome Relay ;)"
                                        }
                        writeChan inChan message
                        stat.tx.hit 1

    traverse_ run (zip3 peers stats [1000, 2000 ..])
    void $ forkIO doWork
    forever showStat
