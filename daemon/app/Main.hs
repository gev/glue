import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, replicateM_)
import Data.UUID.V4 (nextRandom)
import Network.WebSockets (runClient)
import Reacthome.Daemon.App (application)

main :: IO ()
main = do
    let port = 3003
        host = "0.0.0.0"
    putStrLn $ "Connect to Reacthome Relay on " <> host <> ":" <> show port
    replicateM_ 1_000 $ run host port
    forever $ threadDelay 10_000
  where
    run host port =
        forkIO do
            peer <- nextRandom
            let path = "/" <> show peer
            runClient host port path $ application peer
