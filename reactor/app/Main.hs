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
            , "(device 123 :broken t)" -- Тут будет ошибка
            ]
    forM_ inputs \input -> do
        TIO.putStrLn $ "Testing: " <> input
        case parseReactor input of
            Left err -> print err -- Печатаем готовую красивую ошибку
            Right ast -> print ast
        TIO.putStrLn "---"
