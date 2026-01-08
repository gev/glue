module Glue.Lib.Math.Utility.Trunc where

import Data.Scientific (toRealFloat)
import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

trunc :: [IR Eval] -> Eval (IR Eval)
trunc [arg] = do
    va <- evalRequired arg
    case va of
        Integer n -> pure $ Integer n
        Float n -> pure $ Integer (Prelude.truncate n)
        _ -> throwError $ wrongArgumentType ["number"]
trunc _ = throwError wrongNumberOfArguments
