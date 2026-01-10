module Glue.Eval (
    Eval,
    EvalState (..),
    eval,
    runEval,
    runEvalLegacy,
    evalRequired,
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
    liftIO,
    apply,
    isCallable,
) where

import Control.Monad (ap, liftM)
import Data.List (inits)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
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
data EvalState = EvalState
    { env :: Env
    , context :: Context
    , registry :: ModuleRegistry Eval
    , importCache :: ImportedModuleCache Eval
    , rootEnv :: Env
    }

newtype Eval a = Eval
    { runEvalInternal :: EvalState -> IO (Either Error (a, EvalState))
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

getEnv :: Eval Env
getEnv = Eval $ \state -> pure $ Right (state.env, state)

putEnv :: Env -> Eval ()
putEnv newEnv = Eval $ \state -> pure $ Right ((), state{env = newEnv})

getRootEnv :: Eval Env
getRootEnv = Eval $ \state -> pure $ Right (state.rootEnv, state)

putRootEnv :: Env -> Eval ()
putRootEnv newRootEnv = Eval $ \state -> pure $ Right ((), state{rootEnv = newRootEnv})

getState :: Eval EvalState
getState = Eval $ \state -> pure $ Right (state, state)

putState :: EvalState -> Eval ()
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

throwError :: Exception -> Eval a
throwError err = Eval $ \state -> pure $ Left (EvalError state.context err)

liftIO :: IO a -> Eval a
liftIO action = Eval $ \state -> do
    a <- action
    pure $ Right (a, state)

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
    results <- mapM evalRequired xs
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
    evaluatedMap <- mapM evalRequired objMap
    pure $ IR.Object evaluatedMap

-- Evaluate literal values (numbers, strings, etc.)
evalLiteral :: IR -> Eval IR
evalLiteral v = pure v

-- Evaluate arguments for function calls
evalArguments :: [IR] -> Eval [IR]
evalArguments rawArgs = mapM evalRaw rawArgs

-- Apply a native function/command/special
applyNative :: IR.Native Eval -> [IR] -> Eval IR
applyNative (IR.Func f) rawArgs = do
    args <- evalArguments rawArgs
    f args
applyNative (IR.Special s) rawArgs = s rawArgs

-- Apply a closure with the given arguments
applyClosure :: [Text] -> IR -> Env -> [IR] -> Eval IR
applyClosure params body savedEnv rawArgs = do
    argValues <- evalArguments rawArgs
    let numArgs = length argValues
        numParams = length params

    if numArgs == numParams
        then do
            -- Full application: execute the function
            let bindings = zip params argValues
            let newEnv = buildEnvWithBindings savedEnv bindings
            withSavedEnv newEnv (evalBody body)
        else
            if numArgs < numParams
                then do
                    -- Partial application: create new closure with remaining params
                    let (usedParams, remainingParams) = splitAt numArgs params
                    let bindings = zip usedParams argValues
                    let partiallyAppliedEnv = buildEnvWithBindings savedEnv bindings
                    pure $ IR.Closure remainingParams body partiallyAppliedEnv
                else throwError wrongNumberOfArguments

-- Evaluate function body with implicit sequence semantics
evalBody :: IR -> Eval IR
evalBody (IR.List elements) = do
    -- Function bodies with lists are implicit sequences - evaluate all and return last
    results <- mapM eval elements
    case results of
        [] -> pure IR.Void -- Empty sequence returns void
        xs -> pure $ last xs
evalBody other = eval other

-- Normalize final results by unwrapping single-element lists
normalizeResult :: Maybe IR -> Maybe IR
normalizeResult (Just (IR.List [single])) = Just single
normalizeResult result = result

-- Evaluate without normalization (for arguments)
evalRaw :: IR -> Eval IR
evalRaw ir = case ir of
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
    _ -> evalLiteral ir

eval :: IR -> Eval IR
eval = evalRaw

apply :: IR -> [IR] -> Eval IR
apply ir rawArgs = case ir of
    IR.Native native -> applyNative native rawArgs
    IR.Closure params body savedEnv -> applyClosure params body savedEnv rawArgs
    IR.Symbol name -> throwError $ unboundVariable name
    _ -> throwError notCallableObject

evalRequired :: IR -> Eval IR
evalRequired = evalRaw

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

runEval :: Eval a -> EvalState -> IO (Either Error (a, EvalState))
runEval (Eval f) = f

-- | Legacy runEval for backward compatibility (deprecated)
runEvalLegacy :: Eval a -> Env -> IO (Either Error (a, Env, Context))
runEvalLegacy evalAction initialEnv = do
    let initialState =
            EvalState
                { env = initialEnv
                , context = []
                , registry = emptyRegistry -- Empty registry for legacy compatibility
                , importCache = Map.empty
                , rootEnv = initialEnv
                }
    result <- runEval evalAction initialState
    case result of
        Left err -> pure $ Left err
        Right (a, finalState) -> pure $ Right (a, finalState.env, finalState.context)
