module Reactor.Eval where

import Control.Monad (ap, liftM)
import Data.Maybe (catMaybes)
import Data.Text (Text)

import Reactor.Env qualified as E
import Reactor.Error (ReactorError (..))
import Reactor.Value qualified as V

newtype Eval a = Eval
    { runEval :: V.Env Eval -> IO (Either ReactorError (a, V.Env Eval))
    }

type Value = V.Value Eval
type Env = V.Env Eval

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

eval :: Value -> Eval (Maybe Value)
eval (V.VSymbol name) = do
    env <- getEnv
    case E.lookupVar name env of
        Right val -> pure $ Just val
        Left err -> throwError err
eval (V.VList xs) = do
    results <- mapM eval xs
    let clean = catMaybes results
    case clean of
        (f : args) | isCallable f -> apply f args
        _ -> pure . Just . V.VList $ clean
  where
    isCallable (V.VNative _) = True
    isCallable (V.VClosure{}) = True
    isCallable _ = False
eval (V.VCall name rawArgs) = do
    env <- getEnv
    case E.lookupVar name env of
        Right func -> apply func rawArgs
        Left err -> throwError err
eval v = pure $ Just v

apply :: Value -> [Value] -> Eval (Maybe Value)
apply (V.VNative native) rawArgs = case native of
    V.VFunc f -> do
        args <- catMaybes <$> mapM eval rawArgs
        Just <$> f args
    V.VCmd c -> do
        args <- catMaybes <$> mapM eval rawArgs
        c args >> pure Nothing
    V.VSpecial s -> s rawArgs
apply (V.VClosure params body savedEnv) rawArgs = do
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
apply (V.VSymbol name) _ = throwError $ SyntaxError ("Unbound variable: " <> name)
apply _ _ = throwError $ SyntaxError "Not a callable object"

evalRequired :: Value -> Eval Value
evalRequired v =
    eval v >>= \case
        Just val -> pure val
        Nothing -> throwError $ SyntaxError "Expected value, but got a command/effect"

defineVarEval :: Text -> Value -> Eval ()
defineVarEval name val = do
    env <- getEnv
    putEnv (E.defineVar name val env)

updateVarEval :: Text -> Value -> Eval ()
updateVarEval name val = do
    env <- getEnv
    case E.updateVar name val env of
        Right nextEnv -> putEnv nextEnv
        Left err -> throwError err
