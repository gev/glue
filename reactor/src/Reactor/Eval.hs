module Reactor.Eval where

import Control.Monad (ap, liftM)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Reactor.Env qualified as E
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR qualified as IR

type IR = IR.IR Eval
type Env = IR.Env Eval

newtype Eval a = Eval
    { runEval :: Env -> IO (Either EvalError (a, Env))
    }

instance Functor Eval where
    fmap = liftM

instance Applicative Eval where
    pure a = Eval $ \env -> pure $ Right (a, env)
    (<*>) = ap

instance Monad Eval where
    (Eval m) >>= f = Eval $ \env -> do
        res <- m env
        case res of
            Left err -> pure $ Left err
            Right (a, env') -> runEval (f a) env'

getEnv :: Eval Env
getEnv = Eval $ \env -> pure $ Right (env, env)

putEnv :: Env -> Eval ()
putEnv newEnv = Eval $ \_ -> pure $ Right ((), newEnv)

throwError :: EvalError -> Eval a
throwError err = Eval $ \_ -> pure $ Left err

liftIO :: IO a -> Eval a
liftIO action = Eval $ \env -> do
    a <- action
    pure $ Right (a, env)

-- Helper to determine if an IR value can be called
isCallable :: IR -> Bool
isCallable (IR.Native _) = True
isCallable (IR.Closure{}) = True
isCallable _ = False

-- Helper for managing environment frames during function calls
withSavedEnv :: Env -> Eval a -> Eval a
withSavedEnv savedEnv action = do
    currentEnv <- getEnv
    putEnv savedEnv
    result <- action
    putEnv currentEnv
    pure result

-- Safely build environment with parameter bindings
buildEnvWithBindings :: Env -> [(Text, IR)] -> Env
buildEnvWithBindings savedEnv = foldl defineBinding (E.pushFrame savedEnv)
  where
    defineBinding env (param, value) = E.defineVar param value env

-- Safely extract single parameter from parameter list
extractSingleParam :: [Text] -> Maybe Text
extractSingleParam [param] = Just param
extractSingleParam _ = Nothing

-- Safely create bindings from object map and parameters
createNamedBindings :: Map Text IR -> [Text] -> [(Text, IR)]
createNamedBindings objMap = map createBinding
  where
    createBinding param =
        ( param
        , fromMaybe
            (error "impossible: param not in map")
            (Map.lookup param objMap)
        )

-- Evaluate a symbol by looking it up in the environment
evalSymbol :: Text -> Eval (Maybe IR)
evalSymbol name = do
    env <- getEnv
    case E.lookupVar name env of
        Right val -> pure $ Just val
        Left err -> throwError err

-- Evaluate a list (function call or literal list)
evalList :: [IR] -> Eval (Maybe IR)
evalList (IR.Symbol name : rawArgs) = do
    env <- getEnv
    case E.lookupVar name env of
        Right func -> apply func rawArgs
        Left err -> throwError err
evalList xs = do
    results <- mapM eval xs
    let clean = catMaybes results
    case clean of
        (f : args) | isCallable f -> apply f args
        _ -> pure . Just . IR.List $ clean

-- Evaluate property access on an object
evalPropAccess :: IR -> Text -> Eval (Maybe IR)
evalPropAccess objExpr prop = do
    objVal <- evalRequired objExpr
    case objVal of
        IR.Object objMap -> case Map.lookup prop objMap of
            Just val -> pure $ Just val
            Nothing -> throwError $ PropertyNotFound prop
        _ -> throwError $ NotAnObject (T.pack $ show objVal)

-- Evaluate literal values (numbers, strings, etc.)
evalLiteral :: IR -> Eval (Maybe IR)
evalLiteral v = pure $ Just v

-- Evaluate arguments for function calls
evalArguments :: [IR] -> Eval [IR]
evalArguments rawArgs = catMaybes <$> mapM eval rawArgs

-- Apply a native function/command/special
applyNative :: IR.Native Eval -> [IR] -> Eval (Maybe IR)
applyNative (IR.Func f) rawArgs = do
    args <- evalArguments rawArgs
    Just <$> f args
applyNative (IR.Cmd c) rawArgs = do
    args <- evalArguments rawArgs
    c args >> pure Nothing
applyNative (IR.Special s) rawArgs = s rawArgs

-- Apply a closure with the given arguments
applyClosure :: [Text] -> IR -> Env -> [IR] -> Eval (Maybe IR)
applyClosure params body savedEnv rawArgs = do
    argValues <- evalArguments rawArgs
    if length params /= length argValues
        then throwError WrongNumberOfArguments
        else do
            let bindings = zip params argValues
            let newEnv = buildEnvWithBindings savedEnv bindings
            withSavedEnv newEnv (eval body)

eval :: IR -> Eval (Maybe IR)
eval (IR.Symbol name) = evalSymbol name
eval (IR.List xs) = evalList xs
eval (IR.PropAccess objExpr prop) = evalPropAccess objExpr prop
eval v = evalLiteral v

apply :: IR -> [IR] -> Eval (Maybe IR)
apply (IR.Native native) rawArgs = applyNative native rawArgs
apply (IR.Closure params body savedEnv) rawArgs = applyClosure params body savedEnv rawArgs
apply (IR.Symbol name) _ = throwError $ UnboundVariable name
apply _ _ = throwError NotCallableObject

evalRequired :: IR -> Eval IR
evalRequired v =
    eval v >>= \case
        Just val -> pure val
        Nothing -> throwError ExpectedValue

defineVarEval :: Text -> IR -> Eval ()
defineVarEval name val = do
    env <- getEnv
    putEnv (E.defineVar name val env)

updateVarEval :: Text -> IR -> Eval ()
updateVarEval name val = do
    env <- getEnv
    case E.updateVar name val env of
        Right nextEnv -> putEnv nextEnv
        Left err -> throwError err
