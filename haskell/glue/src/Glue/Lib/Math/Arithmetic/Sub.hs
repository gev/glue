module Glue.Lib.Math.Arithmetic.Sub where

import Glue.Eval.Exception

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.IR (IR (..))

sub :: [IR Eval] -> Eval (IR Eval)
sub [] = throwError $ wrongArgumentType ["number"]
sub [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (-n)
        _ -> throwError $ wrongArgumentType ["number"]
sub args = do
    values <- mapM evalRequired args
    case values of
        [] -> throwError $ wrongArgumentType ["number"]
        (Number first : rest) -> do
            let nums = first : [n | Number n <- rest]
            if length nums /= length values
                then throwError $ wrongArgumentType ["number"]
                else pure $ Number (foldl (-) first (tail nums))
        _ -> throwError $ wrongArgumentType ["number"]
