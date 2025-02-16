import Network.Wai.Handler.Warp
import Reacthome.Auth.App
import Reacthome.Auth.Environment
import Reacthome.Auth.Repository.InMemory.AuthFlows
import Reacthome.Auth.Repository.InMemory.AuthUsers
import Reacthome.Auth.Repository.InMemory.Credential.PublicKeys
import Reacthome.Auth.Repository.InMemory.Users

main :: IO ()
main = do
  let port = 3000
  let ?environment =
        Environment
          { name = "Reacthome Auth Service"
          , domain = "reacthome.net"
          , challengeSize = 20
          , timeout = 600
          }
  authFlows <- makeAuthFlows
  let ?authFlows = authFlows
  authUsers <- makeAuthUsers
  let ?authUsers = authUsers
  users <- makeUsers
  let ?users = users
  publicKeys <- makePublicKeys
  let ?publicKeys = publicKeys
  putStrLn $ "Serving on port " <> show port
  run port app
