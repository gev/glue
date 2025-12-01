import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently_, race_)
import Control.Exception (catch)
import Control.Monad (forever, replicateM, void, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (unpack)
import Data.Text.Format.Numbers (prettyI)
import Data.UUID.V4 (nextRandom)
import GHC.IORef (atomicModifyIORef'_)
import Reacthome.Daemon.App (application)
import Reacthome.Relay.Stat (RelayHits (hits), RelayStat (..), makeRelayStat)
import System.Clock (Clock (..), diffTimeSpec, getTime, toNanoSecs)
import Web.WebSockets.Client (runWebSocketClient)
import Web.WebSockets.Error (WebSocketError)
import Prelude hiding (last)

concurrency :: Int
concurrency = 16_000

main :: IO ()
main = do
    stats <- replicateM concurrency makeRelayStat
    total <- newIORef (0, 0)
    time <- newIORef =<< getTime Monotonic
    connections <- newIORef @Int 0
    let
        port = 3003
        host = "172.16.1.1"

        run (stat, delay) = do
            threadDelay delay
            void $ atomicModifyIORef'_ connections (+ 1)
            catch @WebSocketError
                do
                    let ?stat = stat
                    peer <- nextRandom
                    let path = "/" <> show peer
                    -- putStrLn $ "Connect to Reacthome Relay on " <> host <> ":" <> show port <> path
                    runWebSocketClient host port path $ application peer
                print
            void $ atomicModifyIORef'_ connections \n -> n - 1

        summarize x = sum <$> traverse (hits . x) stats

        summarizeStat = do
            r <- summarize rx
            t <- summarize tx
            pure (r, t)

        rps x1 x0 dt = fmt x1 <> " " <> " | RPS: " <> fmt ((x1 - x0) `div` dt)

        fmt = unpack . prettyI (Just '.')

        showStat = forever do
            t0 <- readIORef time
            t1 <- getTime Monotonic
            writeIORef time t1

            (rx0, tx0) <- readIORef total
            (rx1, tx1) <- summarizeStat
            writeIORef total (rx1, tx1)

            let !dt = fromInteger $ toNanoSecs (diffTimeSpec t1 t0) `div` 1_000_000_000

            n <- readIORef connections

            when (dt > 0) do
                putStrLn $ "<-> " <> fmt n <> " connections"
                putStrLn $ "Tx: " <> rps tx1 tx0 dt
                putStrLn $ "Rx: " <> rps rx1 rx0 dt

            threadDelay 1_000_000

    race_
        do mapConcurrently_ run $ zip stats [100_000, 200_000 ..]
        showStat
