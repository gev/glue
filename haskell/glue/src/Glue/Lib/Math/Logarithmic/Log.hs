module Glue.Lib.Math.Logarithmic.Log where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception (RuntimeException (..))
import Glue.IR (IR (..))

log :: [IR Eval] -> Eval (IR Eval)
log [arg, base] = do
    va <- evalRequired arg
    vb <- evalRequired base
    case (va, vb) of
        (Number n, Number b) -> do
            let realVal = toRealFloat @Double n
            let realBase = toRealFloat @Double b
            if realVal <= 0 || realBase <= 0 || realBase == 1
                then throwError $ WrongArgumentType ["positive number", "positive number != 1"]
                else pure $ Number (fromFloatDigits (Prelude.logBase realBase realVal))
        _ -> throwError $ WrongArgumentType ["number", "number"]
log _ = throwError WrongNumberOfArguments
