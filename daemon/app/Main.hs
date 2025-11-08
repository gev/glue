main :: IO ()
main = do
    let port = 3003 :: Int
        host = "0.0.0.0"
    putStrLn $ "Connect to Reacthome Relay on " <> host <> ":" <> show port
