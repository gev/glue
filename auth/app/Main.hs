import JOSE.KeyPair
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Static
import Reacthome.Auth.App
import Reacthome.Auth.Environment
import Reacthome.Auth.Repository.InMemory.AuthFlows
import Reacthome.Auth.Repository.InMemory.AuthUsers
import Reacthome.Auth.Repository.InMemory.Credential.PublicKeys
import Reacthome.Auth.Repository.InMemory.RefreshTokens
import Reacthome.Auth.Repository.SQLite.Users
import Util.SQLite

main :: IO ()
main = do
  let port = 3002
  let ?environment =
        Environment
          { name = "Reacthome Auth Service"
          , domain = "reacthome.net"
          , challengeSize = 20
          , authTimeout = 100
          , authFlowCookieTTL = 300
          , authCodeTTL = 30
          , accessTokenTTL = 900
          }
  authFlows <- makeAuthFlows
  let ?authFlows = authFlows
  authUsers <- makeAuthUsers
  let ?authUsers = authUsers
  auth <- makePool "./var/reacthome-auth.db" 100 10
  users <- makeUsers auth
  let ?users = users
  publicKeys <- makePublicKeys
  let ?publicKeys = publicKeys
  keyPair <- generateKeyPair
  let ?keyPair = keyPair
  refreshTokens <- makeRefreshTokens
  let ?refreshTokens = refreshTokens
  putStrLn $ "Serving Reacthome Auth on port " <> show port
  run port $
    staticPolicy
      (addBase "auth/public")
      app
