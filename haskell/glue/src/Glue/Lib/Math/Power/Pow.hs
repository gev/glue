module Glue.Lib.Math.Power.Pow where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

pow :: [IR Eval] -> Eval (IR Eval)
pow [arg1, arg2] = do
    va1 <- evalRequired arg1
    va2 <- evalRequired arg2
    case (va1, va2) of
        (Integer n1, Integer n2) -> pure $ Integer (n1 ^ n2)
        (Integer n1, Float n2) -> pure $ Float (fromIntegral n1 ** n2)
        (Float n1, Integer n2) -> pure $ Float (n1 ** fromIntegral n2)
        (Float n1, Float n2) -> pure $ Float (n1 ** n2)
        _ -> throwError $ wrongArgumentType ["number", "number"]
pow _ = throwError wrongNumberOfArguments
