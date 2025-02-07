import Network.Wai.Handler.Warp
import Reacthome.Auth.App
import Reacthome.Auth.Environment
import Reacthome.Auth.Repository.InMemory.Challenges
import Reacthome.Auth.Repository.InMemory.Credential.PublicKeys
import Reacthome.Auth.Repository.InMemory.Users

main :: IO ()
main = do
  let ?environment =
        Environment
          { name = "Reacthome Auth Service"
          , domain = "reacthome.net"
          , timeout
          , challengeSize = 20
          }
  challenges <- makeChallenges
  users <- makeUsers
  publicKeys <- makePublicKeys
  let ?challenges = challenges
  let ?users = users
  let ?publicKeys = publicKeys
  let port = 3000
  putStrLn $ "Serving on port " <> show port
  run port app
 where
  timeout = 60_000
