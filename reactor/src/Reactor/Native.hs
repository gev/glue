module Reactor.Native (
    initialEnv,
) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Reactor.Env qualified as E
import Reactor.Eval (Eval, defineVarEval, evalRequired, getEnv, throwError, updateVarEval)
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR (Env, IR (..), Native (..))

type V = IR Eval

builtinQuote :: [V] -> Eval (Maybe V)
builtinQuote args = case E.makeQuote args of
    Right val -> pure (Just val)
    Left err -> throwError err

builtinDef :: [V] -> Eval (Maybe V)
builtinDef [Symbol name, rawVal] = do
    val <- evalRequired rawVal
    defineVarEval name val
    pure Nothing
builtinDef _ = throwError DefExpectedSymbolAndValue

builtinSet :: [V] -> Eval (Maybe V)
builtinSet [Symbol name, rawVal] = do
    val <- evalRequired rawVal
    updateVarEval name val
    pure Nothing
builtinSet [PropAccess (Symbol objName) prop, rawVal] = do
    val <- evalRequired rawVal
    -- Get current object
    env <- getEnv
    case E.lookupVar objName env of
        Right currentObj -> case currentObj of
            Object objMap -> do
                let newMap = Map.insert prop val objMap
                let newObj = Object newMap
                updateVarEval objName newObj
                pure Nothing
            List xs -> case listToObject xs of
                Just objMap -> do
                    let newMap = Map.insert prop val objMap
                    let newObj = Object newMap
                    updateVarEval objName newObj
                    pure Nothing
                Nothing -> throwError $ NotAnObject (T.pack $ show currentObj)
            _ -> throwError $ NotAnObject (T.pack $ show currentObj)
        Left err -> throwError err
builtinSet _ = throwError SetExpectedSymbolAndValue

builtinLambda :: [V] -> Eval (Maybe V)
builtinLambda [argsNode, body] = do
    rawArgs <- case argsNode of
        List xs -> pure xs
        _ -> throwError LambdaExpectedArgumentsList
    params <- case E.extractSymbols rawArgs of
        Right ps -> pure ps
        Left err -> throwError err
    capturedEnv <- getEnv
    pure . Just $ E.makeClosure params body capturedEnv
builtinLambda _ = throwError LambdaExpectedArgumentsAndBody

builtinList :: [V] -> Eval V
builtinList args = pure (List args)

listToObject :: [V] -> Maybe (Map.Map Text V)
listToObject = go Map.empty
  where
    go acc [] = Just acc
    go acc (Symbol k : v : rest) | T.isPrefixOf ":" k = go (Map.insert (T.drop 1 k) v acc) rest
    go _ _ = Nothing

initialEnv :: Env Eval
initialEnv =
    E.fromList
        [ ("quote", Native (Special builtinQuote))
        , ("def", Native (Special builtinDef))
        , ("set", Native (Special builtinSet))
        , ("lambda", Native (Special builtinLambda))
        , ("list", Native (Func builtinList))
        ]
