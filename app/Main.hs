import Network.Wai.Handler.Warp
import Reacthome.Auth.App
import Reacthome.Auth.Environment
import Reacthome.Auth.Repository.InMemory.RegisterChallenges

main :: IO ()
main = do
  let ?environment =
        Environment
          { name = "Reacthome Auth Service"
          , domain = "reacthome.net"
          , timeout
          , challengeSize = 20
          }
  challenges <- mkRegisterChallenges
  let ?challenges = challenges
  let port = 3000
  putStrLn $ "Serving on port " <> show port
  run port app
 where
  timeout = 60_000
