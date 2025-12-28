module Reactor.Lib.List.Nth where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

nth :: [IR Eval] -> Eval (IR Eval)
nth [indexIR, listIR] = do
    indexVal <- evalRequired indexIR
    listVal <- evalRequired listIR
    case (indexVal, listVal) of
        (Number idx, List xs) -> do
            let intIdx = floor idx
            if intIdx < 0 || intIdx >= length xs
                then throwError $ WrongArgumentType ["valid index"]
                else pure $ xs !! intIdx
        _ -> throwError $ WrongArgumentType ["number", "list"]
nth _ = throwError WrongNumberOfArguments
