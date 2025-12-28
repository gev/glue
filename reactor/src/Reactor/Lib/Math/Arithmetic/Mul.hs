module Reactor.Lib.Math.Arithmetic.Mul where

import Reactor.Eval.Error (GeneralError (..))

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.IR (IR (..))

mul :: [IR Eval] -> Eval (IR Eval)
mul [] = throwError $ WrongArgumentType ["number"]
mul args = do
    values <- mapM evalRequired args
    case values of
        [] -> throwError $ WrongArgumentType ["number"]
        (Number first : rest) -> do
            let nums = first : [n | Number n <- rest]
            if length nums /= length values
                then throwError $ WrongArgumentType ["number"]
                else pure $ Number (product nums)
        _ -> throwError $ WrongArgumentType ["number"]
