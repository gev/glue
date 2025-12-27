module Reactor.Lib.Builtin.Lambda where

import Data.Text (Text)
import Reactor.Eval (Eval, getEnv, throwError)
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR (Env, IR (..))

lambda :: [IR Eval] -> Eval (Maybe (IR Eval))
lambda [argsNode, body] = do
    rawArgs <- case argsNode of
        List xs -> pure xs
        _ -> throwError LambdaExpectedArgumentsList
    params <- case extractSymbols rawArgs of
        Right ps -> pure ps
        Left err -> throwError err
    capturedEnv <- getEnv
    pure . Just $ makeClosure params body capturedEnv
lambda _ = throwError LambdaExpectedArgumentsAndBody

makeClosure :: [Text] -> IR m -> Env m -> IR m
makeClosure = Closure

extractSymbols :: [IR m] -> Either EvalError [Text]
extractSymbols = mapM \case
    Symbol s -> Right s
    _ -> Left ExpectedListOfSymbols
