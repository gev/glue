module Main where

import Control.Monad (forM_)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Glue.Env qualified as E
import Glue.Eval (Eval, EvalState (..), eval, runEval)
import Glue.IR (IR (..), compile)
import Glue.Lib (lib)
import Glue.Parser (parseGlue)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runInteractive
        ["test"] -> runTests
        _ -> putStrLn "Usage: glue [test]"

runInteractive :: IO ()
runInteractive = do
    let initialEnv = E.fromFrame lib
    let initialState =
            EvalState
                { env = initialEnv
                , context = []
                }

    -- Example usage - evaluate sequentially with state updates
    let examples =
            [ "(+ 1 2 3)"
            , "(* 2 3)"
            , "(:name \"Alice\" :age 30)"
            , "(def x 42)"
            , "x"
            , "(if (> x 40) \"big\" \"small\")"
            ]

    evalSequentially examples initialState

evalSequentially :: [String] -> EvalState -> IO ()
evalSequentially [] _ = pure ()
evalSequentially (input : rest) currentState = do
    TIO.putStrLn $ "Evaluating: " <> T.pack input
    case parseGlue (T.pack input) of
        Left err -> do
            print err
            evalSequentially rest currentState
        Right ast -> do
            result <- runEval (eval (compile ast)) currentState
            case result of
                Right (val, newState) -> do
                    print val
                    evalSequentially rest newState
                Left err -> do
                    print err
                    evalSequentially rest currentState
    TIO.putStrLn "---"

runTests :: IO ()
runTests = do
    let inputs =
            [ "(+ 1 2 3)"
            , "(:name \"Alice\" :age 30)"
            , "(def x 42)"
            , "x"
            ]
    forM_ inputs \input -> do
        TIO.putStrLn $ "Testing: " <> T.pack input
        case parseGlue (T.pack input) of
            Left err -> print err -- Print the formatted error
            Right ast -> print ast
        TIO.putStrLn "---"
