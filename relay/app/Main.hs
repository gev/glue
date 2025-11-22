import Reacthome.Relay.App (application)
import Reacthome.Relay.Server (makeRelayServer)
import Web.WebSockets.Server (runWebSocketServer)

main :: IO ()
main =
  run "0.0.0.0" 3003
 where
  run host port = do
    putStrLn $ "Run Reacthome Relay on " <> host <> ":" <> show port
    runWebSocketServer host port $ application makeRelayServer
