module Glue.Lib.Bool.Lt where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

lt :: [IR Eval] -> Eval (IR Eval)
lt [a, b] = do
    va <- evalRequired a
    vb <- evalRequired b
    case (va, vb) of
        (Number na, Number nb) -> pure . Bool $ na < nb
        _ -> throwError $ WrongArgumentType ["number", "number"]
lt _ = throwError $ WrongArgumentType ["number", "number"]
