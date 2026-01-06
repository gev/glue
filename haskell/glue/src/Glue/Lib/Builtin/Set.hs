module Glue.Lib.Builtin.Set where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Glue.Env (lookupVar)
import Glue.Eval (Eval, evalRequired, getEnv, throwError, updateVarEval)
import Glue.Eval.Exception (RuntimeException (..))
import Glue.IR (IR (..))

set :: [IR Eval] -> Eval (Maybe (IR Eval))
set [Symbol name, rawVal] = do
    val <- evalRequired rawVal
    let parts = T.splitOn "." name
    case parts of
        [] -> throwError $ WrongArgumentType ["target", "value"] -- shouldn't happen
        [varName] -> do
            updateVarEval varName val
            pure Nothing
        [objName, prop] -> do
            env <- getEnv
            case lookupVar objName env of
                Right currentObj -> case currentObj of
                    Object objMap -> do
                        let newMap = Map.insert prop val objMap
                        let newObj = Object newMap
                        updateVarEval objName newObj
                        pure Nothing
                    Module _ -> throwError CannotModifyModule
                    _ -> throwError $ NotAnObject (T.pack $ show currentObj)
                Left err -> throwError err
        _ -> throwError $ WrongArgumentType ["target", "value"]
set _ = throwError $ WrongArgumentType ["target", "value"]
