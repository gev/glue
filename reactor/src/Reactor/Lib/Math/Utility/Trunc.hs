module Reactor.Lib.Math.Utility.Trunc where

import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR (IR (..))

trunc :: [IR Eval] -> Eval (IR Eval)
trunc [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromIntegral (truncate (toRealFloat n)))
        _ -> throwError TruncExpectedOneNumber
trunc _ = throwError WrongNumberOfArguments
