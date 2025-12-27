module Reactor.Lib.Math.Arithmetic.Sub where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR (IR (..))

sub :: [IR Eval] -> Eval (IR Eval)
sub [] = throwError SubExpectedAtLeastOneArgument
sub [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (-n)
        _ -> throwError SubExpectedNumbers
sub args = do
    values <- mapM evalRequired args
    case values of
        [] -> throwError SubExpectedAtLeastOneArgument
        (Number first : rest) -> do
            let nums = first : [n | Number n <- rest]
            if length nums /= length values
                then throwError SubExpectedNumbers
                else pure $ Number (foldl (-) first (tail nums))
        _ -> throwError SubExpectedNumbers
