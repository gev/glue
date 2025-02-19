import Network.Wai.Handler.Warp
import Reacthome.Assist.App
import Reacthome.Assist.Environment

main :: IO ()
main = do
  let port = 3001
  let ?environment =
        Environment
          { gate =
              GateConfig
                { host = "gate.reacthome.net"
                , port = 443
                , protocol = "client"
                }
          }
  putStrLn $ "Serving Reacthome Assist on port " <> show port
  run port app
