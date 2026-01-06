module Glue.Lib.Builtin.Error where

import Glue.Eval (Eval, defineVarEval, evalRequired, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

errorFunc :: [IR Eval] -> Eval (Maybe (IR Eval))
errorFunc [Symbol name, rawVal] = do
    val <- evalRequired rawVal
    defineVarEval name val
    pure . Just $ Exception name val
errorFunc _ = throwError $ wrongArgumentType ["symbol", "value"]
