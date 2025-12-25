module Reactor.Eval where

import Control.Monad (ap, liftM)
import Data.Maybe (catMaybes)
import Data.Text (Text)

import Reactor.Env qualified as E
import Reactor.Error (ReactorError (..))
import Reactor.IR qualified as V

type IR = V.IR Eval
type Env = V.Env Eval

newtype Eval a = Eval
    { runEval :: Env -> IO (Either ReactorError (a, Env))
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

throwError :: ReactorError -> Eval a
throwError err = Eval $ \_ -> pure $ Left err

liftIO :: IO a -> Eval a
liftIO action = Eval $ \env -> do
    a <- action
    pure $ Right (a, env)

eval :: IR -> Eval (Maybe IR)
eval (V.Symbol name) = do
    env <- getEnv
    case E.lookupVar name env of
        Right val -> pure $ Just val
        Left err -> throwError err
eval (V.List (V.Symbol name : rawArgs)) = do
    env <- getEnv
    case E.lookupVar name env of
        Right func -> apply func rawArgs
        Left err -> throwError err
eval (V.List xs) = do
    results <- mapM eval xs
    let clean = catMaybes results
    case clean of
        (f : args) | isCallable f -> apply f args
        _ -> pure . Just . V.List $ clean
  where
    isCallable (V.Native _) = True
    isCallable (V.Closure{}) = True
    isCallable _ = False
eval v = pure $ Just v

apply :: IR -> [IR] -> Eval (Maybe IR)
apply (V.Native native) rawArgs = case native of
    V.Func f -> do
        args <- catMaybes <$> mapM eval rawArgs
        Just <$> f args
    V.Cmd c -> do
        args <- catMaybes <$> mapM eval rawArgs
        c args >> pure Nothing
    V.Special s -> s rawArgs
apply (V.Closure params body savedEnv) rawArgs = do
    argValues <- catMaybes <$> mapM eval rawArgs
    if length params /= length argValues
        then throwError (SyntaxError "Wrong number of arguments")
        else do
            currentEnv <- getEnv
            let newEnv = foldr (\(p, v) e -> E.defineVar p v e) (E.pushFrame savedEnv) (zip params argValues)
            putEnv newEnv
            res <- eval body
            putEnv currentEnv
            pure res
apply (V.Symbol name) _ = throwError $ SyntaxError ("Unbound variable: " <> name)
apply _ _ = throwError $ SyntaxError "Not a callable object"

evalRequired :: IR -> Eval IR
evalRequired v =
    eval v >>= \case
        Just val -> pure val
        Nothing -> throwError $ SyntaxError "Expected value, but got a command/effect"

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
