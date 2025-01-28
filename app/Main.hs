import App
import Environment
import Network.Wai.Handler.Warp
import Repository.InMemory.RegisterChallenges

main :: IO ()
main = do
    challenges <- mkRegisterChallenges timeout
    let ?environment =
            Environment
                { name = "Reacthome Auth Service"
                , domain = "reacthome.net"
                , timeout
                }
    let ?challenges = challenges
    let port = 3000
    putStrLn $ "Serving on port " <> show port
    run port app
  where
    timeout = 60_000
