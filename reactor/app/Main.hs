module Main where

import Control.Monad (forM_)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Reactor.Env qualified as E
import Reactor.Eval (Eval, EvalState (..), eval, runEval)
import Reactor.IR (IR (..), compile)
import Reactor.Lib (lib)
import Reactor.Module (RegisteredModule(..))
import Reactor.Module.Cache qualified as Cache
import Reactor.Module.Loader qualified as Loader
import Reactor.Module.Registry qualified as Registry
import Reactor.Parser (parseReactor)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runInteractive
        ["test"] -> runTests
        ["load", dir] -> loadAndTestModules dir
        _ -> putStrLn "Usage: reactor [test | load <dir>]"

runInteractive :: IO ()
runInteractive = do
    -- Load modules from examples directory
    registryResult <- Loader.loadModulesFromDirectory "examples"
    case registryResult of
        Left err -> putStrLn $ "Failed to load modules: " ++ err
        Right modules -> do
            putStrLn $ "Loaded " ++ show (length modules) ++ " modules"
            -- Build registry from loaded modules
            let registry = case Loader.buildRegistryFromModules modules of
                    Left regErr -> error $ "Registry build failed: " ++ show regErr
                    Right reg -> reg
            let initialEnv = E.fromFrame lib
            let initialState = EvalState
                    { env = initialEnv
                    , context = []
                    , registry = registry
                    , importCache = Cache.emptyCache
                    , rootEnv = initialEnv
                    }

            -- Example usage - evaluate sequentially with state updates
            let examples =
                    [ "(import math.utils)"
                    , "(add-ten 5)"
                    , "(multiply-by-two 3)"
                    , "(import list.utils)"
                    , "(first-and-last (list 1 2 3 4 5))"
                    , "(sum-list (list 1 2 3 4 5))"
                    ]

            evalSequentially examples initialState

evalSequentially :: [String] -> EvalState -> IO ()
evalSequentially [] _ = pure ()
evalSequentially (input:rest) currentState = do
    TIO.putStrLn $ "Evaluating: " <> T.pack input
    case parseReactor (T.pack input) of
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
            [ "(light :id 1 :state :on)"
            , "(device 123 :broken t)" -- This will cause an error
            ]
    forM_ inputs \input -> do
        TIO.putStrLn $ "Testing: " <> T.pack input
        case parseReactor (T.pack input) of
            Left err -> print err -- Print the formatted error
            Right ast -> print ast
        TIO.putStrLn "---"

loadAndTestModules :: FilePath -> IO ()
loadAndTestModules dir = do
    result <- Loader.loadModulesFromDirectory dir
    case result of
        Left err -> putStrLn $ "Error loading modules: " ++ err
        Right modules -> do
            putStrLn $ "Successfully loaded " ++ show (length modules) ++ " modules:"
            forM_ modules \(RegisteredModule {name = n, exports = e}) -> do
                putStrLn $ "  - " ++ T.unpack n ++ " (exports: " ++ show e ++ ")"
