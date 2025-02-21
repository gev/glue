import Network.Wai.Handler.Warp
import Reacthome.Assist.App
import Reacthome.Assist.Controller.Dialog.Answer
import Reacthome.Assist.Environment
import Reacthome.Assist.Repository.Answers
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
          }
  answers <- makeAnswers
  let ?answers = answers
  gateConnectionPool <- makeConnectionPool handleAnswer
  let ?gateConnectionPool = gateConnectionPool
  putStrLn $ "Serving Reacthome Assist on port " <> show port
  run port app
