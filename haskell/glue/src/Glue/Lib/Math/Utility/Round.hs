module Glue.Lib.Math.Utility.Round where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

-- Round function
-- Mirrors Haskell Glue.Lib.Math.Utility.Round.round exactly
round :: IR Eval
round = NativeFunc roundImpl

-- Round function implementation
-- Mirrors Haskell Glue.Lib.Math.Utility.Round.roundImpl exactly
roundImpl :: [IR Eval] -> Eval (IR Eval)
roundImpl [arg] = do
    va <- eval arg
    case va of
        Integer n -> pure $ Integer n
        Float n -> pure $ Integer (Prelude.round n)
        _ -> throwError $ wrongArgumentType ["number"]
roundImpl _ = throwError wrongNumberOfArguments
