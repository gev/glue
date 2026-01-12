module Glue.Lib.Builtin.Lambda (lambda, makeClosure, extractSymbols) where

import Data.Text (Text)
import Glue.Eval (Eval, getEnv, throwError)
import Glue.Eval.Exception (RuntimeException, expectedListOfSymbols, wrongArgumentType)
import Glue.IR (IR (..))

lambda :: [IR Eval] -> Eval (IR Eval)
lambda [argsNode, body] = do
    rawArgs <- case argsNode of
        List xs -> pure xs
        _ -> throwError $ wrongArgumentType ["arguments list", "body"]
    params <- case extractSymbols rawArgs of
        Right ps -> pure ps
        Left _ -> throwError $ wrongArgumentType ["symbols in arguments", "body"]
    makeClosure params body
lambda _ = throwError $ wrongArgumentType ["arguments", "body"]

makeClosure :: [Text] -> IR Eval -> Eval (IR Eval)
makeClosure params body =
    Closure params body <$> getEnv

extractSymbols :: [IR m] -> Either (RuntimeException m) [Text]
extractSymbols = mapM \case
    Symbol s -> Right s
    _ -> Left expectedListOfSymbols
