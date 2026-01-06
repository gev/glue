module Glue.Lib.Math.Logarithmic.Ln where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

ln :: [IR Eval] -> Eval (IR Eval)
ln [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> do
            let realVal = toRealFloat @Double n
            if realVal <= 0
                then throwError $ wrongArgumentType ["positive number"]
                else pure $ Number (fromFloatDigits (Prelude.log realVal))
        _ -> throwError $ wrongArgumentType ["number"]
ln _ = throwError $ wrongArgumentType ["number"]
