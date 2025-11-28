import Control.Concurrent (forkOS, runInBoundThread, threadDelay)
import Control.Monad (forever, void)
import Reacthome.Relay.App (application)
import Reacthome.Relay.Dispatcher (makeRelayDispatcher)
import Reacthome.Relay.Server (makeRelayServer)
import Web.WebSockets.Server (runWebSocketServer)

main :: IO ()
main =
  run "0.0.0.0" 3003
 where
  run host port = do
    dispatcher <- makeRelayDispatcher
    let server = makeRelayServer dispatcher
    putStrLn $ "Run Reacthome Relay on " <> host <> ":" <> show port
    runWebSocketServer host port $ application server
