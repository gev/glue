module Reactor.Module.Import where

import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Reactor.Env qualified as E
import Reactor.Eval (Eval, eval, getEnv, liftIO, putEnv, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (Frame, IR (..), Native (..))
import Reactor.Module (ImportedModule (..), ImportedModuleCache, Module (..))
import Reactor.Module.Registration (RegistryRef)
import Prelude hiding (mod)

-- | Global cache reference for imported modules
type ImportedCacheRef m = IORef (ImportedModuleCache m)

-- | Create a new empty imported module cache
newImportedCache :: IO (ImportedCacheRef m)
newImportedCache = newIORef Map.empty

-- | Import special form - loads and evaluates a module with caching
importForm :: RegistryRef Eval -> ImportedCacheRef Eval -> [IR Eval] -> Eval (Maybe (IR Eval))
importForm registry cache [Symbol moduleName] = do
    -- Check if module already imported (cached)
    cacheMap <- liftIO $ readIORef cache
    case Map.lookup moduleName cacheMap of
        Just imported -> do
            -- Use cached results - merge into current environment
            currentEnv <- getEnv
            let updatedEnv = foldl (\env (name, val) -> E.defineVar name val env) currentEnv (Map.toList (exportedValues imported))
            putEnv updatedEnv
            pure Nothing
        Nothing -> do
            -- First import: lookup module in registry
            registryMap <- liftIO $ readIORef registry
            case Map.lookup moduleName registryMap of
                Nothing -> throwError $ ModuleNotFound moduleName
                Just mod -> do
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

                    -- Cache the imported module
                    let importedModule =
                            ImportedModule
                                { moduleName = moduleName
                                , exportedValues = exportedValues
                                , evaluationRootEnv = rootEnv
                                }
                    liftIO $ modifyIORef cache (Map.insert moduleName importedModule)

                    -- Merge exported symbols into current environment
                    let updatedEnv = foldl (\env (name, val) -> E.defineVar name val env) rootEnv (Map.toList exportedValues)
                    putEnv updatedEnv

                    pure Nothing
importForm _ _ _ = throwError $ WrongArgumentType ["module-name"]

-- | Create import function for builtin environment
importFunc :: RegistryRef Eval -> ImportedCacheRef Eval -> Frame Eval
importFunc registry cache =
    Map.fromList
        [ ("import", Native (Special (importForm registry cache)))
        ]
