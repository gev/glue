module Reactor.Lib.List.Take where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

take :: [IR Eval] -> Eval (IR Eval)
take [countIR, listIR] = do
    count <- evalRequired countIR
    list <- evalRequired listIR
    case (count, list) of
        (Number n, List xs) -> do
            if n < 0
                then throwError $ WrongArgumentType ["non-negative integer"]
                else pure $ List (Prelude.take (floor n) xs)
        _ -> throwError $ WrongArgumentType ["number", "list"]
take _ = throwError WrongNumberOfArguments
