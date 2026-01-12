module Glue.Lib.Math.Logarithmic.Ln where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

ln :: [IR Eval] -> Eval (IR Eval)
ln [arg] = do
    va <- eval arg
    case va of
        Integer n -> pure $ Float (log (fromIntegral n))
        Float n -> pure $ Float (log n)
        _ -> throwError $ wrongArgumentType ["number"]
ln _ = throwError wrongNumberOfArguments
