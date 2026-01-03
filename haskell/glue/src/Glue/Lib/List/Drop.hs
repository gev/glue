module Glue.Lib.List.Drop where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

drop :: [IR Eval] -> Eval (IR Eval)
drop [countIR, listIR] = do
    count <- evalRequired countIR
    list <- evalRequired listIR
    case (count, list) of
        (Number n, List xs) -> do
            if n < 0
                then throwError $ WrongArgumentType ["non-negative integer"]
                else pure $ List (Prelude.drop (floor n) xs)
        _ -> throwError $ WrongArgumentType ["number", "list"]
drop _ = throwError WrongNumberOfArguments
