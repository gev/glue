module Glue.Lib.Builtin.Lambda where

import Data.Text (Text)
import Glue.Eval (Eval, getEnv, throwError)
import Glue.Eval.Exception (RuntimeException (..))
import Glue.IR (Env, IR (..))

lambda :: [IR Eval] -> Eval (Maybe (IR Eval))
lambda [argsNode, body] = do
    rawArgs <- case argsNode of
        List xs -> pure xs
        _ -> throwError $ WrongArgumentType ["arguments list", "body"]
    params <- case extractSymbols rawArgs of
        Right ps -> pure ps
        Left _ -> throwError $ WrongArgumentType ["symbols in arguments", "body"]
    capturedEnv <- getEnv
    pure . Just $ makeClosure params body capturedEnv
lambda _ = throwError $ WrongArgumentType ["arguments", "body"]

makeClosure :: [Text] -> IR m -> Env m -> IR m
makeClosure = Closure

extractSymbols :: [IR m] -> Either RuntimeException [Text]
extractSymbols = mapM \case
    Symbol s -> Right s
    _ -> Left ExpectedListOfSymbols
