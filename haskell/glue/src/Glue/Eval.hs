module Glue.Eval (
    Eval,
    Runtime,
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
    getRuntime,
    putRuntime,
    getRegistry,
    getCache,
    putCache,
    defineVarEval,
    updateVarEval,
) where

import Control.Monad (ap, liftM)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Glue.Env qualified as E
import Glue.Eval.Error (EvalError (EvalError))
import Glue.Eval.Exception
import Glue.IR qualified as IR
import Glue.Module.Cache (ImportedModuleCache)
import Glue.Module.Registry (ModuleRegistry, emptyRegistry)
import Glue.Runtime qualified as GR

type IR = IR.IR Eval
type Env = IR.Env Eval
type Error = EvalError Eval
type Exception = RuntimeException Eval
type Runtime = GR.Runtime Eval

newtype Eval a = Eval
    { runEval :: Runtime -> IO (Either Error (a, Runtime))
    }

instance Functor Eval where
    fmap = liftM

instance Applicative Eval where
    pure a = Eval $ \runtime -> pure $ Right (a, runtime)
    (<*>) = ap

instance Monad Eval where
    (Eval m) >>= f = Eval $ \runtime -> do
        res <- m runtime
        case res of
            Left err -> pure $ Left err
            Right (a, runtime') -> runEval (f a) runtime'

-- | Simple runEval with empty module registry
runEvalSimple :: Eval a -> Env -> IO (Either Error (a, Runtime))
runEvalSimple evalAction initialEnv = do
    let initialState =
            GR.Runtime
                { GR.env = initialEnv
                , GR.context = []
                , GR.registry = emptyRegistry
                , GR.importCache = Map.empty
                , GR.rootEnv = initialEnv
                }
    runEval evalAction initialState

throwError :: Exception -> Eval a
throwError err = Eval $ \runtime -> pure $ Left (EvalError (GR.context runtime) err)

liftIO :: IO a -> Eval a
liftIO action = Eval $ \runtime -> do
    a <- action
    pure $ Right (a, runtime)

-- Evaluate
eval :: IR -> Eval IR
eval ir = case ir of
    IR.Symbol name -> evalSymbol name
    IR.DottedSymbol parts -> evalDottedSymbol parts
    IR.List xs -> evalList xs
    IR.Object objMap -> evalObject objMap
    _ -> pure ir

-- Evaluate a symbol by looking it up in the environment
evalSymbol :: Text -> Eval IR
evalSymbol name = do
    env <- getEnv
    case E.lookupVar name env of
        Right val -> case val of
            IR.Evaluable value -> do
                pushContext name
                result <- eval =<< value
                popContext
                pure result
            _ -> pure val
        Left err -> throwError err

-- Evaluate dotted symbol access
evalDottedSymbol :: [Text] -> Eval IR
evalDottedSymbol parts = do
    case parts of
        [] -> throwError $ unboundVariable "" -- shouldn't happen
        [base] -> evalSymbol base
        base : rest -> do
            env <- getEnv
            case E.lookupVar base env of
                Left err -> throwError err
                Right obj -> evalNestedAccess obj rest
  where
    evalNestedAccess obj [] = pure obj
    evalNestedAccess obj (prop : rest) = do
        case obj of
            IR.Object objMap -> case Map.lookup prop objMap of
                Just val -> evalNestedAccess val rest
                Nothing -> throwError $ propertyNotFound prop
            IR.NativeValue hv -> case Map.lookup prop (IR.getters hv) of
                Just getter -> do
                    -- Execute the getter action directly
                    result <- getter
                    evalNestedAccess result rest
                Nothing -> throwError $ propertyNotFound prop
            _ -> throwError $ notAnObject obj

-- Evaluate a list (function call or literal list)
evalList :: [IR] -> Eval IR
evalList [IR.Symbol name] = evalSymbol name
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

-- Apply multiple arguments sequentially for currying
applyNativeFunc0 :: (IR -> Eval IR) -> [IR] -> Eval IR
applyNativeFunc0 func [] = pure $ IR.NativeFunc func -- No args, return function as-is
applyNativeFunc0 func (arg : rest) = do
    evaluatedArg <- eval arg -- Evaluate argument before passing to function
    result <- func evaluatedArg
    if isCallable result
        then apply result rest -- Apply remaining args to result
        else case rest of
            [] -> pure result
            _ -> throwError wrongNumberOfArguments

-- Evaluate an object
evalObject :: Map Text IR -> Eval IR
evalObject objMap = do
    evaluatedMap <- mapM eval objMap
    pure $ IR.Object evaluatedMap

-- Helper to determine if an IR value can be called
isCallable :: IR -> Bool
isCallable = \case
    IR.NativeFunc _ -> True
    IR.Special _ -> True
    IR.Closure{} -> True
    _ -> False

apply :: IR -> [IR] -> Eval IR
apply ir args = case ir of
    IR.NativeFunc f -> applyNativeFunc0 f args
    IR.Special s -> s args -- Special forms still take lists
    IR.Closure params body savedEnv -> applyClosure params body savedEnv args
    IR.Symbol name -> throwError $ unboundVariable name
    _ -> throwError notCallableObject

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
getEnv = Eval $ \runtime -> pure $ Right (GR.env runtime, runtime)

putEnv :: Env -> Eval ()
putEnv newEnv = Eval $ \runtime -> pure $ Right ((), runtime{GR.env = newEnv})

getRootEnv :: Eval Env
getRootEnv = Eval $ \runtime -> pure $ Right (GR.rootEnv runtime, runtime)

putRootEnv :: Env -> Eval ()
putRootEnv newRootEnv = Eval $ \runtime -> pure $ Right ((), runtime{GR.rootEnv = newRootEnv})

getRuntime :: Eval Runtime
getRuntime = Eval $ \runtime -> pure $ Right (runtime, runtime)

putRuntime :: Runtime -> Eval ()
putRuntime runtime = Eval $ \_ -> pure $ Right ((), runtime)

getRegistry :: Eval (ModuleRegistry Eval)
getRegistry = Eval $ \runtime -> pure $ Right (GR.registry runtime, runtime)

getCache :: Eval (ImportedModuleCache Eval)
getCache = Eval $ \runtime -> pure $ Right (GR.importCache runtime, runtime)

putCache :: ImportedModuleCache Eval -> Eval ()
putCache newCache = Eval $ \runtime -> pure $ Right ((), runtime{GR.importCache = newCache})

pushContext :: Text -> Eval ()
pushContext name = Eval $ \runtime -> pure $ Right ((), runtime{GR.context = name : GR.context runtime})

popContext :: Eval ()
popContext = Eval $ \runtime -> case GR.context runtime of
    (_ : rest) -> pure $ Right ((), runtime{GR.context = rest})
    [] -> pure $ Right ((), runtime) -- shouldn't happen, but safe

-- Helper for managing environment frames during function calls
withEnv :: Env -> Eval a -> Eval a
withEnv env action = do
    currentEnv <- getEnv
    putEnv env
    result <- action
    putEnv currentEnv
    pure result
