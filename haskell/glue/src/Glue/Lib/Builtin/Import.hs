module Glue.Lib.Builtin.Import where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Glue.Env qualified as E
import Glue.Eval (Eval, Runtime (..), eval, getCache, getEnv, getRegistry, getRootEnv, getRuntime, liftIO, putCache, putEnv, runEval, throwError)
import Glue.Eval.Error (EvalError (..))
import Glue.Eval.Exception (moduleNotFound, wrongArgumentType)
import Glue.IR (IR (..))
import Glue.Module (ImportedModule (..), RegisteredModule (..))
import Glue.Module.Cache qualified as Cache

import Glue.Module.Registry qualified as Registry
import Prelude hiding (mod)

-- | Import special form - loads and evaluates a module
importForm :: [IR Eval] -> Eval (IR Eval)
importForm [Symbol moduleName] = importModule [moduleName]
importForm [DottedSymbol modulePath] = importModule modulePath
importForm _ = throwError $ wrongArgumentType ["module-name"]

importModule :: [T.Text] -> Eval (IR Eval)
importModule [] = throwError $ wrongArgumentType ["module-name"]
importModule modulePath = do
    let moduleName = T.intercalate "." modulePath
    registry <- getRegistry
    case Registry.lookupModule moduleName registry of
        Nothing -> throwError $ moduleNotFound moduleName
        Just mod -> do
            cache <- getCache
            -- Check if already imported (cached)
            case Cache.lookupCachedModule moduleName cache of
                Just imported -> do
                    -- Use cached results - add exports directly to current environment
                    env <- getEnv
                    let updatedEnv = foldl (\e (name, val) -> E.defineVar name val e) env (Map.toList (exportedValues imported))
                    putEnv updatedEnv
                    pure Void
                Nothing -> do
                    -- First import: evaluate module
                    -- Get root environment for consistent evaluation
                    rootEnv <- getRootEnv -- Initial env contains root builtins

                    -- Create isolated environment for module evaluation
                    let builtinsFrame = last rootEnv -- Builtins are the bottom frame
                    let isolatedEnv = E.pushFrame [builtinsFrame] -- [temp_frame, builtins]

                    -- Get current evaluation runtime for isolated evaluation
                    currentState <- Glue.Eval.getRuntime

                    -- Create isolated runtime for module evaluation
                    let isolatedState = currentState{Glue.Eval.env = isolatedEnv}

                    -- Evaluate module in complete isolation (doesn't affect current runtime)
                    moduleEvalResult <- liftIO $ runEval (mapM eval mod.body) isolatedState

                    -- Extract exported symbols from isolated evaluation
                    exportedValues <- case moduleEvalResult of
                        Left (EvalError _ innerErr) -> throwError innerErr
                        Right (_, finalIsolatedState) -> do
                            let moduleEnv = Glue.Eval.env finalIsolatedState
                            pure $
                                Map.fromList
                                    [ ( exportName
                                      , case E.lookupVar exportName moduleEnv of
                                            Right val -> val
                                            Left _ -> error $ "Exported symbol not defined: " <> T.unpack exportName
                                      )
                                    | exportName <- mod.exports
                                    ]

                    -- Create imported module record
                    let importedModule =
                            ImportedModule
                                { moduleName = moduleName
                                , exportedValues = exportedValues
                                , evaluationRootEnv = rootEnv
                                }

                    -- Update cache immutably
                    let newCache = Cache.cacheModule importedModule cache
                    putCache newCache

                    -- Add exported symbols directly to current environment
                    env <- getEnv
                    let updatedEnv = foldl (\e (name, val) -> E.defineVar name val e) env (Map.toList exportedValues)
                    putEnv updatedEnv

                    pure Void
