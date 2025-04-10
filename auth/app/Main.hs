import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Static
import Reacthome.Auth.App
import Reacthome.Auth.Environment
import Reacthome.Auth.Repository.AuthFlows
import Reacthome.Auth.Repository.AuthUsers
import Reacthome.Auth.Repository.Clients
import Reacthome.Auth.Repository.Credentials.PublicKeys.SQLite as U
import Reacthome.Auth.Repository.PublicKeys.SQLite as J
import Reacthome.Auth.Repository.RefreshTokens.SQLite
import Reacthome.Auth.Repository.Users.SQLite
import Reacthome.Auth.Service.Secret
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
  usersStore <- makePool "./var/reacthome-auth/db/users.db" 100 10
  keyStore <- makePool "./var/reacthome-auth/db/keys.db" 100 10
  clients <- makeClients "./var/reacthome-auth/clients.json"
  let ?clients = clients
  users <- makeUsers usersStore
  let ?users = users
  userPublicKeys <- U.makePublicKeys usersStore
  let ?userPublicKeys = userPublicKeys
  jwkPublicKeys <- J.makePublicKeys keyStore
  let ?jwkPublicKeys = jwkPublicKeys
  keyPair <- makeSecret
  let ?keyPair = keyPair
  refreshTokens <- makeRefreshTokens usersStore
  let ?refreshTokens = refreshTokens
  putStrLn $ "Serving Reacthome Auth on port " <> show port
  run port $
    staticPolicy
      (addBase "auth/public")
      app
