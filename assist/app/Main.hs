import Network.Wai.Handler.Warp
import Reacthome.Assist.App

main :: IO ()
main = do
  let port = 3000
  putStrLn $ "Serving Reacthome Assist on port " <> show port
  run port app
