import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_, mapConcurrently_)
import Control.Monad (forever, replicateM, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (unpack)
import Data.Text.Format.Numbers (prettyI)
import Data.UUID.V4 (nextRandom)
import Reacthome.Daemon.App (application)
import Reacthome.Relay.Stat (RelayHits (hits), RelayStat (..), makeRelayStat)
import System.Clock (Clock (..), diffTimeSpec, getTime, toNanoSecs)
import Web.WebSockets.Client (runWebSocketClient)
import Prelude hiding (last)

concurrency :: Int
concurrency = 5

main :: IO ()
main = do
    stats <- replicateM concurrency makeRelayStat
    total <- newIORef (0, 0)
    time <- newIORef =<< getTime Monotonic
    let
        port = 3003
        host = "172.16.1.1"

        run stat = do
            let ?stat = stat
            peer <- nextRandom
            let path = "/" <> show peer
            putStrLn $ "Connect to Reacthome Relay on " <> host <> ":" <> show port <> path
            runWebSocketClient host port path $ application peer

        summarize x = sum <$> traverse (hits . x) stats

        summarizeStat = do
            r <- summarize rx
            t <- summarize tx
            pure (r, t)

        rps x1 x0 dt = fmt x1 <> " " <> " | RPS: " <> fmt ((x1 - x0) `div` dt)

        fmt = unpack . prettyI (Just '.')

    concurrently_
        do
            mapConcurrently_ run stats
        do
            forever do
                t0 <- readIORef time
                t1 <- getTime Monotonic
                writeIORef time t1

                (rx0, tx0) <- readIORef total
                (rx1, tx1) <- summarizeStat
                writeIORef total (rx1, tx1)

                let dt = fromInteger $ toNanoSecs (diffTimeSpec t1 t0) `div` 1_000_000_000

                when (dt > 0) do
                    putStrLn "<->"
                    putStrLn $ "Tx: " <> rps tx1 tx0 dt
                    putStrLn $ "Rx: " <> rps rx1 rx0 dt

                threadDelay 1_000_000
