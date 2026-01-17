module Glue.Lib.Builtin.Set where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Glue.Env (lookupVar)
import Glue.Eval (Eval, apply, eval, getEnv, throwError, updateVarEval)
import Glue.Eval.Exception (notAnObject, wrongArgumentType)
import Glue.IR (IR (..), getters, setters)

set :: [IR Eval] -> Eval (IR Eval)
set [target, rawVal] = do
    val <- eval rawVal
    case target of
        Symbol name -> do
            let parts = T.splitOn "." name
            setByParts parts val
        DottedSymbol parts -> setByParts parts val
        _ -> throwError $ wrongArgumentType ["target", "value"]
  where
    setByParts [] _ = throwError $ wrongArgumentType ["target", "value"]
    setByParts [varName] val = updateVarEval varName val >> pure Void
    setByParts (objName : props) val = setNested objName props val

    setNested objName [] val = updateVarEval objName val >> pure Void
    setNested objName [prop] val = setProperty objName prop val
    setNested objName (prop : rest) val = do
        -- For nested access like obj.prop.nested, we need to get obj.prop first, then set on the result
        env <- getEnv
        case lookupVar objName env of
            Right obj -> do
                intermediate <- evalDottedProp obj prop
                setOnIntermediate intermediate rest val
            Left err -> throwError err

    setOnIntermediate intermediate [] val = pure val -- This shouldn't happen
    setOnIntermediate intermediate [prop] val = case intermediate of
        Object objMap -> pure $ Object (Map.insert prop val objMap) -- This is wrong, need to update the original
        NativeValue hv -> case Map.lookup prop (setters hv) of
            Just setterFunc -> apply setterFunc [val] >> pure Void
            Nothing -> throwError $ notAnObject intermediate
        _ -> throwError $ notAnObject intermediate
    setOnIntermediate intermediate (prop : rest) val = do
        nested <- evalDottedProp intermediate prop
        setOnIntermediate nested rest val

    evalDottedProp obj prop = case obj of
        Object objMap -> case Map.lookup prop objMap of
            Just val -> pure val
            Nothing -> throwError $ notAnObject obj
        NativeValue hv -> case Map.lookup prop (getters hv) of
            Just getterFunc -> apply getterFunc []
            Nothing -> throwError $ notAnObject obj
        _ -> throwError $ notAnObject obj

    setProperty objName prop val = do
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
                    Nothing -> throwError $ notAnObject currentObj
                _ -> throwError $ notAnObject currentObj
            Left err -> throwError err
set _ = throwError $ wrongArgumentType ["target", "value"]
