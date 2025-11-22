import Control.Concurrent.Async (race_)
import Reacthome.Relay.App (application)
import Reacthome.Relay.Server (makeRelayServer)
import Web.WebSockets.Server (runWebSocketServer)

main :: IO ()
main = do
    let
        run host port = do
            server <- makeRelayServer
            putStrLn $ "Run Reacthome Relay on " <> host <> ":" <> show port
            runWebSocketServer host port $ application server
    race_
        do run "172.16.1.1" 3003
        do run "192.168.11.210" 3003
