import Reacthome.Relay.App (application)
import Reacthome.Relay.Dispatcher (makeRelayDispatcher)

import Reacthome.Relay.Server (makeRelayServer)
import WebSockets.Options (defaultWebSocketOptions)
import WebSockets.Server (runWebSocketServer)

main :: IO ()
main =
  run "0.0.0.0" 3003
 where
  run host port = do
    let ?options = defaultWebSocketOptions
    dispatcher <- makeRelayDispatcher
    let ?dispatcher = dispatcher
    let server = makeRelayServer
    putStrLn $ "Run Reacthome Relay on " <> host <> ":" <> show port
    runWebSocketServer host port $ application server
