import Control.Concurrent.Async (concurrently_)
import Reacthome.Relay.App (application)
import Reacthome.Relay.Relay (Relay (..), makeRelay)
import Reacthome.Relay.Server (makeRelayServer)
import Web.WebSockets.Server (runWebSocketServer)

main :: IO ()
main =
  run "0.0.0.0" 3003
 where
  run host port = do
    relay <- makeRelay 50_000
    let server = makeRelayServer relay
    putStrLn $ "Run Reacthome Relay on " <> host <> ":" <> show port
    concurrently_
      do runWebSocketServer host port $ application server
      do relay.dispatch
