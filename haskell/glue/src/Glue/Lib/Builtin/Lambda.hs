module Glue.Lib.Builtin.Lambda where

import Data.Text (Text)
import Glue.Eval (Eval, getEnv, throwError)
import Glue.Eval.Exception (RuntimeException, expectedListOfSymbols, wrongArgumentType)
import Glue.IR (Env, IR (..))

lambda :: [IR Eval] -> Eval (Maybe (IR Eval))
lambda [argsNode, body] = do
    rawArgs <- case argsNode of
        List xs -> pure xs
        _ -> throwError $ wrongArgumentType ["arguments list", "body"]
    params <- case extractSymbols rawArgs of
        Right ps -> pure ps
        Left _ -> throwError $ wrongArgumentType ["symbols in arguments", "body"]
    capturedEnv <- getEnv
    pure . Just $ makeClosure params body capturedEnv
lambda _ = throwError $ wrongArgumentType ["arguments", "body"]

makeClosure :: [Text] -> IR m -> Env m -> IR m
makeClosure = Closure

extractSymbols :: [IR m] -> Either (RuntimeException m) [Text]
extractSymbols = mapM \case
    Symbol s -> Right s
    _ -> Left expectedListOfSymbols
