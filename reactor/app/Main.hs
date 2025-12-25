module Main where

import Control.Monad (forM_)
import Data.Text.IO qualified as TIO
import Reactor.Parser (parseReactor)

main :: IO ()
main = do
    -- let path = "var/reactor/devices.reactor"
    -- input <- TIO.readFile path
    -- let ast = parseReactor input
    -- print ast

    let inputs =
            [ "(light :id 1 :state :on)"
            , "(device 123 :broken t)" -- This will cause an error
            ]
    forM_ inputs \input -> do
        TIO.putStrLn $ "Testing: " <> input
        case parseReactor input of
            Left err -> print err -- Print the formatted error
            Right ast -> print ast
        TIO.putStrLn "---"
