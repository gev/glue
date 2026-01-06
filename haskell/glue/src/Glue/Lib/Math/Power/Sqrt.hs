module Glue.Lib.Math.Power.Sqrt where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

sqrt :: [IR Eval] -> Eval (IR Eval)
sqrt [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> do
            let realVal = toRealFloat @Double n
            if realVal < 0
                then throwError $ wrongArgumentType ["number"]
                else pure $ Number (fromFloatDigits (Prelude.sqrt realVal))
        _ -> throwError $ wrongArgumentType ["number"]
sqrt _ = throwError wrongNumberOfArguments
