module Glue.Lib.Math.Utility.Round where

import Data.Scientific (toRealFloat)
import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

round :: [IR Eval] -> Eval (IR Eval)
round [arg] = do
    va <- evalRequired arg
    case va of
        Integer n -> pure $ Integer n
        Float n -> pure $ Integer (Prelude.round n)
        _ -> throwError $ wrongArgumentType ["number"]
round _ = throwError wrongNumberOfArguments
