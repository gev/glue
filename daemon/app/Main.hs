import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_, mapConcurrently_)
import Control.Monad (forever, replicateM)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID.V4 (nextRandom)
import Reacthome.Daemon.App (application)
import Reacthome.Relay.Stat (RelayHits (hits), RelayStat (..), makeRelayStat)
import Web.WebSockets.Client (runWebSocketClient)

concurrency :: Int
concurrency = 5

main :: IO ()
main = do
    stats <- replicateM concurrency makeRelayStat
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

        rps x1 x0 dt = show x1 <> " | " <> show ((x1 - x0) `div` dt) <> " rps"

    concurrently_
        do
            mapConcurrently_ run stats
        do
            forever do
                t0 <- getPOSIXTime
                tx0 <- summarize tx
                rx0 <- summarize rx
                threadDelay 1_000_000
                t1 <- getPOSIXTime
                tx1 <- summarize tx
                rx1 <- summarize rx
                let dt = floor $ t1 - t0
                putStrLn "<->"
                putStrLn $ "Tx: " <> rps tx1 tx0 dt
                putStrLn $ "Rx: " <> rps rx1 rx0 dt
