module Reactor.Module.Import where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Reactor.Env qualified as E
import Reactor.Eval (Eval, eval, getCache, getEnv, getRegistry, putCache, putEnv, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (Frame, IR (..), Native (..))
import Reactor.Module (ImportedModule (..), Module (..))
import Reactor.Module.Cache qualified as Cache
import Prelude hiding (mod)

-- | Import special form - loads and evaluates a module
importForm :: [IR Eval] -> Eval (Maybe (IR Eval))
importForm [Symbol moduleName] = do
    registry <- getRegistry
    case Map.lookup moduleName registry of
        Nothing -> throwError $ ModuleNotFound moduleName
        Just mod -> do
            cache <- getCache
            -- Check if already imported (cached)
            case Cache.lookupCachedModule moduleName cache of
                Just imported -> do
                    -- Use cached results - merge into current environment
                    env <- getEnv
                    let updatedEnv = foldl (\e (name, val) -> E.defineVar name val e) env (Map.toList (exportedValues imported))
                    putEnv updatedEnv
                    pure Nothing
                Nothing -> do
                    -- First import: evaluate module
                    -- Get root environment for consistent evaluation
                    rootEnv <- getEnv -- Current env contains root builtins

                    -- Create isolated environment for module evaluation
                    let builtinsFrame = last rootEnv -- Builtins are the bottom frame
                    let isolatedEnv = E.pushFrame [builtinsFrame] -- [temp_frame, builtins]

                    -- Evaluate module body in isolation
                    putEnv isolatedEnv
                    mapM_ (\form -> eval form >> pure ()) mod.body

                    -- Get the environment after evaluation
                    moduleEnv <- getEnv

                    -- Extract exported symbols
                    let exportedValues =
                            Map.fromList
                                [ ( exportName
                                  , case E.lookupVar exportName moduleEnv of
                                        Right val -> val
                                        Left _ -> error $ "Exported symbol not defined: " <> T.unpack exportName
                                  )
                                | exportName <- mod.exports
                                ]

                    -- Restore original environment
                    putEnv rootEnv

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

                    -- Merge exported symbols into current environment
                    let updatedEnv = foldl (\env (name, val) -> E.defineVar name val env) rootEnv (Map.toList exportedValues)
                    putEnv updatedEnv

                    pure Nothing
importForm _ = throwError $ WrongArgumentType ["module-name"]

-- | Create import function for builtin environment
importFunc :: Frame Eval
importFunc =
    Map.fromList
        [ ("import", Native (Special importForm))
        ]
