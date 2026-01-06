module Glue.Lib.Bool.Not where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception (RuntimeException (..))
import Glue.IR (IR (..))

not_ :: [IR Eval] -> Eval (IR Eval)
not_ [arg] = do
    val <- evalRequired arg
    case val of
        Bool False -> pure $ Bool True
        _ -> pure $ Bool False
not_ _ = throwError $ WrongArgumentType ["arg"]
