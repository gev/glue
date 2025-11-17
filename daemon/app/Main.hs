import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, replicateM_)
import Data.UUID.V4 (nextRandom)
import Reacthome.Daemon.App (application)
import Web.WebSockets.Client (runWebSocketClient)

main :: IO ()
main = do
    let port = 3003
        host = "192.168.11.210"
    replicateM_ 100 $ run host port
    forever $ threadDelay 1_000_000
  where
    run host port = forkIO do
        peer <- nextRandom
        let path = "/" <> show peer
        putStrLn $ "Connect to Reacthome Relay on " <> host <> ":" <> show port <> path
        runWebSocketClient host port path $ application peer
