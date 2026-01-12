module Glue.Eval (
    Eval,
    Runtime (..),
    eval,
    runEval,
    runEvalSimple,
    apply,
    isCallable,
    liftIO,
    throwError,
    getEnv,
    putEnv,
    getRootEnv,
    putRootEnv,
    getState,
    putState,
    getRegistry,
    getCache,
    putCache,
    defineVarEval,
    updateVarEval,
) where

import Control.Monad (ap, liftM)
import Data.List (inits)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Glue.Env qualified as E
import Glue.Eval.Error (Context, EvalError (EvalError))
import Glue.Eval.Exception
import Glue.IR qualified as IR
import Glue.Module.Cache (ImportedModuleCache)
import Glue.Module.Registry (ModuleRegistry, emptyRegistry)

type IR = IR.IR Eval
type Env = IR.Env Eval
type Error = EvalError Eval
type Exception = RuntimeException Eval

-- | Complete evaluation state
data Runtime = Runtime
    { env :: Env
    , context :: Context
    , registry :: ModuleRegistry Eval
    , importCache :: ImportedModuleCache Eval
    , rootEnv :: Env
    }

newtype Eval a = Eval
    { runEvalInternal :: Runtime -> IO (Either Error (a, Runtime))
    }

instance Functor Eval where
    fmap = liftM

instance Applicative Eval where
    pure a = Eval $ \state -> pure $ Right (a, state)
    (<*>) = ap

instance Monad Eval where
    (Eval m) >>= f = Eval $ \state -> do
        res <- m state
        case res of
            Left err -> pure $ Left err
            Right (a, state') -> runEvalInternal (f a) state'

-- | Simple runEval with empty module registry
runEvalSimple :: Eval a -> Env -> IO (Either Error (a, Env, Context))
runEvalSimple evalAction initialEnv = do
    let initialState =
            Runtime
                { env = initialEnv
                , context = []
                , registry = emptyRegistry
                , importCache = Map.empty
                , rootEnv = initialEnv
                }
    result <- runEval evalAction initialState
    case result of
        Left err -> pure $ Left err
        Right (a, finalState) -> pure $ Right (a, finalState.env, finalState.context)

runEval :: Eval a -> Runtime -> IO (Either Error (a, Runtime))
runEval (Eval f) = f

throwError :: Exception -> Eval a
throwError err = Eval $ \state -> pure $ Left (EvalError state.context err)

liftIO :: IO a -> Eval a
liftIO action = Eval $ \state -> do
    a <- action
    pure $ Right (a, state)

-- Evaluate
eval :: IR -> Eval IR
eval ir = case ir of
    IR.Symbol name -> do
        result <- evalSymbol name
        case result of
            Just val -> pure val
            Nothing -> throwError expectedValue
    IR.DottedSymbol parts -> do
        result <- evalDottedSymbol parts
        case result of
            Just val -> pure val
            Nothing -> throwError expectedValue
    IR.List xs -> evalList xs
    IR.Object objMap -> evalObject objMap
    _ -> pure ir

-- Evaluate a symbol by looking it up in the environment
evalSymbol :: Text -> Eval (Maybe IR)
evalSymbol name = do
    env <- getEnv
    case E.lookupVar name env of
        Right val -> pure $ Just val
        Left err -> throwError err

-- Evaluate dotted symbol access
evalDottedSymbol :: [Text] -> Eval (Maybe IR)
evalDottedSymbol parts = do
    case parts of
        [] -> throwError $ unboundVariable "" -- shouldn't happen
        [base] -> evalSymbol base
        _ -> evalWithPrefixes (init $ inits parts) -- All proper prefixes
  where
    -- Try to find the longest prefix that exists as a symbol
    evalWithPrefixes [] = throwError $ unboundVariable (T.intercalate "." parts)
    evalWithPrefixes (prefix : restPrefixes) = do
        let prefixName = T.intercalate "." prefix
        env <- getEnv
        case E.lookupVar prefixName env of
            Right val -> evalNestedAccess val (drop (length prefix) parts)
            Left _ -> evalWithPrefixes restPrefixes

    evalNestedAccess obj [] = pure $ Just obj
    evalNestedAccess obj (prop : rest) = do
        case obj of
            IR.Object objMap -> case Map.lookup prop objMap of
                Just val -> evalNestedAccess val rest
                Nothing -> throwError $ propertyNotFound prop
            IR.Module moduleMap -> case Map.lookup prop moduleMap of
                Just val -> evalNestedAccess val rest
                Nothing -> throwError $ propertyNotFound prop
            _ -> throwError $ notAnObject obj

-- Evaluate a list (function call or literal list)
evalList :: [IR] -> Eval IR
evalList [IR.Symbol name] = do
    pushContext name
    env <- getEnv
    case E.lookupVar name env of
        Right func | isCallable func -> do
            result <- apply func []
            popContext
            pure result
        Right val -> do
            popContext
            pure val
        Left err -> do
            popContext
            throwError err
evalList (IR.Symbol name : rawArgs) = do
    pushContext name
    env <- getEnv
    case E.lookupVar name env of
        Right func -> do
            result <- apply func rawArgs
            popContext
            pure result
        Left err -> do
            popContext
            throwError err
evalList xs = do
    results <- mapM eval xs
    case results of
        (f : args) | isCallable f -> do
            pushContext "<call>"
            result <- apply f args
            popContext
            pure result
        res -> pure $ IR.List res

-- Evaluate an object
evalObject :: Map Text IR -> Eval IR
evalObject objMap = do
    evaluatedMap <- mapM eval objMap
    pure $ IR.Object evaluatedMap

-- Helper to determine if an IR value can be called
isCallable :: IR -> Bool
isCallable (IR.Native _) = True
isCallable (IR.Closure{}) = True
isCallable _ = False

apply :: IR -> [IR] -> Eval IR
apply ir rawArgs = case ir of
    IR.Native native -> applyNative native rawArgs
    IR.Closure params body savedEnv -> applyClosure params body savedEnv rawArgs
    IR.Symbol name -> throwError $ unboundVariable name
    _ -> throwError notCallableObject

-- Apply a native function/command/special
applyNative :: IR.Native Eval -> [IR] -> Eval IR
applyNative (IR.Func f) rawArgs = do
    args <- mapM eval rawArgs
    f args
applyNative (IR.Special s) rawArgs = s rawArgs

-- Apply a closure with the given arguments
applyClosure :: [Text] -> IR -> Env -> [IR] -> Eval IR
applyClosure params body env rawArgs = do
    argValues <- mapM eval rawArgs
    let numArgs = length argValues
        numParams = length params

    if numArgs == numParams
        then do
            -- Full application: execute the function
            let bindings = zip params argValues
            let newEnv = buildEnvWithBindings env bindings
            withEnv newEnv (evalBody body)
        else
            if numArgs < numParams
                then do
                    -- Partial application: create new closure with remaining params
                    let (usedParams, remainingParams) = splitAt numArgs params
                    let bindings = zip usedParams argValues
                    let partiallyAppliedEnv = buildEnvWithBindings env bindings
                    pure $ IR.Closure remainingParams body partiallyAppliedEnv
                else throwError wrongNumberOfArguments

-- Evaluate function body with implicit sequence semantics
evalBody :: IR -> Eval IR
evalBody body =
    eval body >>= \case
        IR.List [] -> pure IR.Void
        IR.List xs -> pure $ last xs
        other -> pure other

-- Safely build environment with parameter bindings
buildEnvWithBindings :: Env -> [(Text, IR)] -> Env
buildEnvWithBindings env = foldl defineBinding (E.pushFrame env)
  where
    defineBinding env' (param, value) = E.defineVar param value env'

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

getEnv :: Eval Env
getEnv = Eval $ \state -> pure $ Right (state.env, state)

putEnv :: Env -> Eval ()
putEnv newEnv = Eval $ \state -> pure $ Right ((), state{env = newEnv})

getRootEnv :: Eval Env
getRootEnv = Eval $ \state -> pure $ Right (state.rootEnv, state)

putRootEnv :: Env -> Eval ()
putRootEnv newRootEnv = Eval $ \state -> pure $ Right ((), state{rootEnv = newRootEnv})

getState :: Eval Runtime
getState = Eval $ \state -> pure $ Right (state, state)

putState :: Runtime -> Eval ()
putState newState = Eval $ \_ -> pure $ Right ((), newState)

getRegistry :: Eval (ModuleRegistry Eval)
getRegistry = Eval $ \state -> pure $ Right (state.registry, state)

getCache :: Eval (ImportedModuleCache Eval)
getCache = Eval $ \state -> pure $ Right (state.importCache, state)

putCache :: ImportedModuleCache Eval -> Eval ()
putCache newCache = Eval $ \state -> pure $ Right ((), state{importCache = newCache})

pushContext :: Text -> Eval ()
pushContext name = Eval $ \state -> pure $ Right ((), state{context = name : state.context})

popContext :: Eval ()
popContext = Eval $ \state -> case state.context of
    (_ : rest) -> pure $ Right ((), state{context = rest})
    [] -> pure $ Right ((), state) -- shouldn't happen, but safe

-- Helper for managing environment frames during function calls
withEnv :: Env -> Eval a -> Eval a
withEnv env action = do
    currentEnv <- getEnv
    putEnv env
    result <- action
    putEnv currentEnv
    pure result
