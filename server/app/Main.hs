main :: IO ()
main = do
    let port = 3004
        host = "0.0.0.0"
    putStrLn $ "Start Reacthome Server on " <> host <> ":" <> show port
