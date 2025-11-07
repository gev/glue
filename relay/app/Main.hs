import Network.WebSockets (runServer)
import Reacthome.Relay.App (application)

main :: IO ()
main = do
    let port = 3003
        host = "0.0.0.0"
    putStrLn $ "Run Reacthome Relay on " <> host <> ":" <> show port
    runServer host port application
