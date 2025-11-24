import Control.Concurrent.Async (concurrently_)
import Reacthome.Relay.App (application)
import Reacthome.Relay.Dispatcher (RelayDispatcher (..), makeRelayDispatcher)
import Reacthome.Relay.Server (makeRelayServer)
import Web.WebSockets.Server (runWebSocketServer)

main :: IO ()
main =
  run "0.0.0.0" 3003
 where
  run host port = do
    dispatcher <- makeRelayDispatcher 100_000
    let server = makeRelayServer dispatcher
    putStrLn $ "Run Reacthome Relay on " <> host <> ":" <> show port
    concurrently_
      do runWebSocketServer host port $ application server
      do dispatcher.run
