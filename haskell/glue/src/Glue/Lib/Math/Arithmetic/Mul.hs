module Glue.Lib.Math.Arithmetic.Mul where

import Glue.Eval.Error (GeneralError (..))

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.IR (IR (..))

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
