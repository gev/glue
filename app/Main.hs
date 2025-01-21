import App
import Environment
import Network.Wai.Handler.Warp

main :: IO ()
main = do
    let ?environment =
            Environment
                { name = "Reacthome Auth Service"
                , domain = "reacthome.net"
                , timeout = 60_000
                }
    let port = 3000
    putStrLn $ "Serving on port " <> show port
    run port app
