module Glue.Lib.Builtin.Set where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Glue.Env (lookupVar)
import Glue.Eval (Eval, eval, getEnv, throwError, updateVarEval)
import Glue.Eval.Exception (cannotModifyModule, notAnObject, wrongArgumentType)
import Glue.IR (IR (..))

set :: [IR Eval] -> Eval (IR Eval)
set [Symbol name, rawVal] = do
    val <- eval rawVal
    let parts = T.splitOn "." name
    case parts of
        [] -> throwError $ wrongArgumentType ["target", "value"]
        [varName] -> do
            updateVarEval varName val
            pure Void
        [objName, prop] -> do
            env <- getEnv
            case lookupVar objName env of
                Right currentObj -> case currentObj of
                    Object objMap -> do
                        let newMap = Map.insert prop val objMap
                        let newObj = Object newMap
                        updateVarEval objName newObj
                        pure Void
                    Module _ -> throwError cannotModifyModule
                    _ -> throwError $ notAnObject currentObj
                Left err -> throwError err
        _ -> throwError $ wrongArgumentType ["target", "value"]
set _ = throwError $ wrongArgumentType ["target", "value"]
