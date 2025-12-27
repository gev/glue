module Reactor.Lib.Math.Arithmetic.Mul where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR (IR (..))

mul :: [IR Eval] -> Eval (IR Eval)
mul [] = throwError MulExpectedAtLeastOneArgument
mul args = do
    values <- mapM evalRequired args
    case values of
        [] -> throwError MulExpectedAtLeastOneArgument
        (Number first : rest) -> do
            let nums = first : [n | Number n <- rest]
            if length nums /= length values
                then throwError MulExpectedNumbers
                else pure $ Number (product nums)
        _ -> throwError MulExpectedNumbers
