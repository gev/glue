module Glue.Lib.Math.Logarithmic.Lg where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

lg :: [IR Eval] -> Eval (IR Eval)
lg [arg] = do
    va <- evalRequired arg
    case va of
        Integer n -> pure $ Float (logBase 10 (fromIntegral n))
        Float n -> pure $ Float (logBase 10 n)
        _ -> throwError $ wrongArgumentType ["number"]
lg _ = throwError wrongNumberOfArguments
