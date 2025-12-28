module Reactor.Lib.Math.Arithmetic.Add where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

add :: [IR Eval] -> Eval (IR Eval)
add [] = throwError $ WrongArgumentType ["number"]
add args = do
    values <- mapM evalRequired args
    case values of
        [] -> throwError $ WrongArgumentType ["number"]
        (Number first : rest) -> do
            let nums = first : [n | Number n <- rest]
            if length nums /= length values
                then throwError $ WrongArgumentType ["number"]
                else pure $ Number (sum nums)
        _ -> throwError $ WrongArgumentType ["number"]
