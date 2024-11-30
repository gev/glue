module App where

import           Control.Exception
import           Network.HTTP.Types
import           Network.Wai

app :: Application
app _ respond = bracket_
    (putStrLn "Allocating scarce resource")
    (putStrLn "Cleaning up")
    (respond $ responseLBS status200 [(hContentType, "text/plain")] "Hello World")
