module Glue.Lib.List.Member where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

member :: [IR Eval] -> Eval (IR Eval)
member [itemIR, listIR] = do
    item <- evalRequired itemIR
    list <- evalRequired listIR
    case list of
        List xs -> pure . Bool $ item `elem` xs
        _ -> throwError $ WrongArgumentType ["list"]
member _ = throwError WrongNumberOfArguments
