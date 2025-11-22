import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_, mapConcurrently_, race_)
import Control.Monad (forever, replicateM)
import Data.UUID.V4 (nextRandom)
import Reacthome.Daemon.App (rxApplication, txApplication)
import Reacthome.Relay.Stat (RelayHits (hits), RelayStat (..), makeRelayStat)
import Web.WebSockets.Client (runWebSocketClient)

concurrency :: Int
concurrency = 2

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
            race_
                do runWebSocketClient host port path $ txApplication peer
                do runWebSocketClient host port path rxApplication

        summarize x = sum <$> traverse (hits . x) stats

        rps x1 x0 = show x1 <> " | " <> show (x1 - x0) <> " rps"

    concurrently_
        do
            mapConcurrently_ run stats
        do
            forever do
                tx0 <- summarize tx
                rx0 <- summarize rx
                threadDelay 1_000_000
                tx1 <- summarize tx
                rx1 <- summarize rx
                putStrLn $ "Tx: " <> rps tx1 tx0
                putStrLn $ "Rx: " <> rps rx1 rx0
