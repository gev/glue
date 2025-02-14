import Network.Wai.Handler.Warp
import Reacthome.Auth.App
import Reacthome.Auth.Environment
import Reacthome.Auth.Repository.InMemory.AuthUsers
import Reacthome.Auth.Repository.InMemory.Credential.PublicKeys
import Reacthome.Auth.Repository.InMemory.Users

main :: IO ()
main = do
  let ?environment =
        Environment
          { name = "Reacthome Auth Service"
          , domain = "reacthome.net"
          , challengeSize = 20
          , timeout = 60_000
          }
  authUsers <- makeAuthUsers
  users <- makeUsers
  publicKeys <- makePublicKeys
  let ?authUsers = authUsers
  let ?users = users
  let ?publicKeys = publicKeys
  let port = 3000
  putStrLn $ "Serving on port " <> show port
  run port app
