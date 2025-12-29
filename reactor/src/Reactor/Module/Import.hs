module Reactor.Module.Import where

import Data.IORef (readIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Reactor.Env qualified as E
import Reactor.Eval (Eval, eval, getEnv, liftIO, putEnv, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (Frame, IR (..), Native (..))
import Reactor.Lib qualified as Lib
import Reactor.Module (Module (..), ModuleRegistry)
import Reactor.Module.Registration (RegistryRef)

-- | Import special form - loads and evaluates a module
importForm :: RegistryRef Eval -> [IR Eval] -> Eval (Maybe (IR Eval))
importForm registry [Symbol moduleName] = do
    -- Lookup module in registry
    registryMap <- liftIO $ readIORef registry
    case Map.lookup moduleName registryMap of
        Nothing -> throwError $ ModuleNotFound moduleName
        Just mod -> do
            -- Evaluate module body in isolated environment
            currentEnv <- getEnv

            -- Create isolated environment for module evaluation
            -- Include builtins but not user code
            let isolatedEnv = E.pushFrame (E.fromFrame Lib.lib) -- [temp_frame, builtins]

            -- Evaluate each form in the module body
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
            putEnv currentEnv

            -- Merge exported symbols into current environment
            let updatedEnv = foldl (\env (name, val) -> E.defineVar name val env) currentEnv (Map.toList exportedValues)
            putEnv updatedEnv

            pure Nothing
importForm _ _ = throwError $ WrongArgumentType ["module-name"]

-- | Create import function for builtin environment
importFunc :: RegistryRef Eval -> Frame Eval
importFunc registry =
    Map.fromList
        [ ("import", Native (Special (importForm registry)))
        ]
