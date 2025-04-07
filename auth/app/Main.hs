import JOSE.KeyPair
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Static
import Reacthome.Auth.App
import Reacthome.Auth.Environment
import Reacthome.Auth.Repository.AuthFlows
import Reacthome.Auth.Repository.AuthUsers
import Reacthome.Auth.Repository.Credentials.PublicKeys.SQLite
import Reacthome.Auth.Repository.RefreshTokens.SQLite
import Reacthome.Auth.Repository.Users.SQLite
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
          , accessTokenTTL = 120
          }
  authFlows <- makeAuthFlows
  let ?authFlows = authFlows
  authUsers <- makeAuthUsers
  let ?authUsers = authUsers
  authStore <- makePool "./var/reacthome-auth.db" 100 10
  users <- makeUsers authStore
  let ?users = users
  publicKeys <- makePublicKeys authStore
  let ?publicKeys = publicKeys
  keyPair <- generateKeyPair
  let ?keyPair = keyPair
  refreshTokens <- makeRefreshTokens authStore
  let ?refreshTokens = refreshTokens
  putStrLn $ "Serving Reacthome Auth on port " <> show port
  run port $
    staticPolicy
      (addBase "auth/public")
      app
