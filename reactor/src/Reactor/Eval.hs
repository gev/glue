module Reactor.Eval where

import Control.Monad (ap, liftM)
import Data.Maybe (catMaybes)
import Data.Text (Text)
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

eval :: IR -> Eval (Maybe IR)
eval (IR.Symbol name) = do
    env <- getEnv
    case E.lookupVar name env of
        Right val -> pure $ Just val
        Left err -> throwError err
eval (IR.List (IR.Symbol name : rawArgs)) = do
    env <- getEnv
    case E.lookupVar name env of
        Right func -> apply func rawArgs
        Left err -> throwError err
eval (IR.List xs) = do
    results <- mapM eval xs
    let clean = catMaybes results
    case clean of
        (f : args) | isCallable f -> apply f args
        _ -> pure . Just . IR.List $ clean
  where
    isCallable (IR.Native _) = True
    isCallable (IR.Closure{}) = True
    isCallable _ = False
eval v = pure $ Just v

apply :: IR -> [IR] -> Eval (Maybe IR)
apply (IR.Native native) rawArgs = case native of
    IR.Func f -> do
        args <- catMaybes <$> mapM eval rawArgs
        Just <$> f args
    IR.Cmd c -> do
        args <- catMaybes <$> mapM eval rawArgs
        c args >> pure Nothing
    IR.Special s -> s rawArgs
apply (IR.Closure params body savedEnv) rawArgs = do
    argValues <- catMaybes <$> mapM eval rawArgs
    if length params /= length argValues
        then throwError WrongNumberOfArguments
        else do
            currentEnv <- getEnv
            let newEnv = foldr (\(p, v) e -> E.defineVar p v e) (E.pushFrame savedEnv) (zip params argValues)
            putEnv newEnv
            res <- eval body
            putEnv currentEnv
            pure res
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
