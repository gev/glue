module Main where

import           App
import           Network.Wai.Handler.Warp

main :: IO ()
main =  run 8080 app
