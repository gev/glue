module Reactor.Lib.Math.Logarithmic.Ln where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))
import Reactor.Lib.Math.Error (MathError (..))

ln :: [IR Eval] -> Eval (IR Eval)
ln [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> do
            let realVal = toRealFloat @Double n
            if realVal <= 0
                then throwError LogExpectedPositiveNumber
                else pure $ Number (fromFloatDigits (Prelude.log realVal))
        _ -> throwError LogExpectedPositiveNumber
ln _ = throwError WrongNumberOfArguments
