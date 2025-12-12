import Network.Wai.Handler.Warp (run)
import Reacthome.Assist.App (app)
import Reacthome.Assist.Controller.Dialog.Answer (handleAnswer)
import Reacthome.Assist.Environment (Environment (..), GateConfig (..))
import Reacthome.Assist.Repository.Answers (makeAnswers)
import Reacthome.Assist.Repository.PublicKeys (makePublicKeys)
import Reacthome.Assist.Repository.Users (makeUsers)
import Reacthome.Assist.Service.JOSE.PublicKey (runPublicKeysUpdate)
import Reacthome.Gate.Connection.Pool (makeConnectionPool)

main :: IO ()
main = do
  let port = 3001
  let ?environment =
        Environment
          { gate =
              GateConfig
                { host = "gate.reacthome.net"
                , port = 443
                , protocol = "connect"
                }
          , queueSize = 42
          , jwksURL = "https://auth.reacthome.net/.well-known/jwks.json"
          , publicKeysUpdateInterval = 60
          }
  answers <- makeAnswers
  let ?answers = answers
  gateConnectionPool <- makeConnectionPool handleAnswer
  let ?gateConnectionPool = gateConnectionPool
  publicKeys <- makePublicKeys
  let ?publicKeys = publicKeys
  runPublicKeysUpdate
  users <- makeUsers "./var/assist.json"
  let ?users = users
  putStrLn $ "Serving Reacthome Assist on port " <> show port
  run port app
