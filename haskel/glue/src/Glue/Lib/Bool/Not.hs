module Glue.Lib.Bool.Not where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

not_ :: [IR Eval] -> Eval (IR Eval)
not_ [arg] = do
    val <- evalRequired arg
    case val of
        Symbol "false" -> pure $ Symbol "true"
        _ -> pure $ Symbol "false"
not_ _ = throwError $ WrongArgumentType ["arg"]
