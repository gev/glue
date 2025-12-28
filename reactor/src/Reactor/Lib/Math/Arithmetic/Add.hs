module Reactor.Lib.Math.Arithmetic.Add where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

add :: [IR Eval] -> Eval (IR Eval)
add [] = throwError WrongNumberOfArguments
add args = do
    values <- mapM evalRequired args
    case values of
        [] -> throwError WrongNumberOfArguments
        (Number first : rest) -> do
            let nums = first : [n | Number n <- rest]
            if length nums /= length values
                then throwError $ WrongArgumentType "+" ["number"]
                else pure $ Number (sum nums)
        _ -> throwError $ WrongArgumentType "+" ["number"]
