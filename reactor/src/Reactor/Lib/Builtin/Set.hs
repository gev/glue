module Reactor.Lib.Builtin.Set where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Reactor.Env (lookupVar)
import Reactor.Eval (Eval, evalRequired, getEnv, throwError, updateVarEval)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

set :: [IR Eval] -> Eval (Maybe (IR Eval))
set [Symbol name, rawVal] = do
    val <- evalRequired rawVal
    updateVarEval name val
    pure Nothing
set [PropAccess (Symbol objName) prop, rawVal] = do
    val <- evalRequired rawVal
    env <- getEnv
    case lookupVar objName env of
        Right currentObj -> case currentObj of
            Object objMap -> do
                let newMap = Map.insert prop val objMap
                let newObj = Object newMap
                updateVarEval objName newObj
                pure Nothing
            _ -> throwError $ NotAnObject (T.pack $ show currentObj)
        Left err -> throwError err
set _ = throwError $ WrongArgumentType ["target", "value"]
