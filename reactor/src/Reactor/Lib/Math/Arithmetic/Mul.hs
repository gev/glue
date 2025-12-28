module Reactor.Lib.Math.Arithmetic.Mul where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (Error)
import Reactor.IR (IR (..))
import Reactor.Lib.Math.Error (MathError (..))

mul :: [IR Eval] -> Eval (IR Eval)
mul [] = throwError MulExpectedAtLeastOneArg
mul args = do
    values <- mapM evalRequired args
    case values of
        [] -> throwError MulExpectedAtLeastOneArg
        (Number first : rest) -> do
            let nums = first : [n | Number n <- rest]
            if length nums /= length values
                then throwError MulExpectedNumbers
                else pure $ Number (product nums)
        _ -> throwError MulExpectedNumbers
