module Reactor.Lib.Math.Utility.Round where

import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR (IR (..))

round :: [IR Eval] -> Eval (IR Eval)
round [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromIntegral (Prelude.round (toRealFloat n)))
        _ -> throwError RoundExpectedOneNumber
round _ = throwError WrongNumberOfArguments
