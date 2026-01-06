module Glue.Lib.Math.Logarithmic.Lg where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

lg :: [IR Eval] -> Eval (IR Eval)
lg [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> do
            let realVal = toRealFloat @Double n
            if realVal <= 0
                then throwError $ wrongArgumentType ["positive number"]
                else pure $ Number (fromFloatDigits (Prelude.logBase 10 realVal))
        _ -> throwError $ wrongArgumentType ["number"]
lg _ = throwError $ wrongArgumentType ["number"]
