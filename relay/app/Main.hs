import Reacthome.Relay.App (application)
import Web.WebSockets.Server (runWebSocketServer)

main :: IO ()
main = do
    let port = 3003
        host = "0.0.0.0"
    putStrLn $ "Run Reacthome Relay on " <> host <> ":" <> show port
    runWebSocketServer host port application
