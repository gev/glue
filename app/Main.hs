import App
import Environment
import Network.Wai.Handler.Warp
import Service.InMemoryChallengeSet

main :: IO ()
main = do
    challenges <- mkInMemoryChallengeSet
    let ?environment =
            Environment
                { name = "Reacthome Auth Service"
                , domain = "reacthome.net"
                , timeout = 60_000
                }
    let ?challenges = challenges
    let port = 3000
    putStrLn $ "Serving on port " <> show port
    run port app
