module Glue.Lib.Builtin.Set where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Glue.Env (lookupVar)
import Glue.Eval (Eval, apply, eval, getEnv, throwError, updateVarEval)
import Glue.Eval.Exception (notAnObject, wrongArgumentType)
import Glue.IR (IR (..), setters)

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
                    NativeValue hv -> case Map.lookup prop (setters hv) of
                        Just setterFunc -> do
                            -- Call the setter function with the new value
                            _ <- apply setterFunc [val]
                            pure Void
                        Nothing -> throwError $ notAnObject currentObj -- or propertyNotFound
                    _ -> throwError $ notAnObject currentObj
                Left err -> throwError err
        _ -> throwError $ wrongArgumentType ["target", "value"]
set _ = throwError $ wrongArgumentType ["target", "value"]
