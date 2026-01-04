module Glue.Lib.Bool.Ge where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

ge :: [IR Eval] -> Eval (IR Eval)
ge [a, b] = do
    va <- evalRequired a
    vb <- evalRequired b
    case (va, vb) of
        (Number na, Number nb) -> pure . Bool $ na >= nb
        _ -> throwError $ WrongArgumentType ["number", "number"]
ge _ = throwError $ WrongArgumentType ["number", "number"]
