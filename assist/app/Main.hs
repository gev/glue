import Network.Wai.Handler.Warp
import Reacthome.Assist.App
import Reacthome.Assist.Controller.Dialog.Answer
import Reacthome.Assist.Environment
import Reacthome.Assist.Repository.Answers
import Reacthome.Assist.Repository.PublicKeys (makePublicKeys)
import Reacthome.Assist.Service.JOSE.PublicKey (runPublicKeysUpdate)
import Reacthome.Gate.Connection.Pool

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
          , jwksURL = "https://dev.auth.reacthome.net/.well-known/jwks.json"
          , publicKeysUpdateInterval = 10
          }
  answers <- makeAnswers
  let ?answers = answers
  gateConnectionPool <- makeConnectionPool handleAnswer
  let ?gateConnectionPool = gateConnectionPool
  publicKeys <- makePublicKeys
  let ?publicKeys = publicKeys
  runPublicKeysUpdate
  putStrLn $ "Serving Reacthome Assist on port " <> show port
  run port app
