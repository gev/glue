module Reactor.Lib.Math.Arithmetic.Add where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.IR (IR (..))
import Reactor.Lib.Math.Error (MathError (..))

add :: [IR Eval] -> Eval (IR Eval)
add [] = throwError AddExpectedAtLeastOneArg
add args = do
    values <- mapM evalRequired args
    case values of
        [] -> throwError AddExpectedAtLeastOneArg
        (Number first : rest) -> do
            let nums = first : [n | Number n <- rest]
            if length nums /= length values
                then throwError AddExpectedNumbers
                else pure $ Number (sum nums)
        _ -> throwError AddExpectedNumbers
