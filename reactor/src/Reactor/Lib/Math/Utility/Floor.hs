module Reactor.Lib.Math.Utility.Floor where

import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR (IR (..))

floor :: [IR Eval] -> Eval (IR Eval)
floor [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromIntegral (Prelude.floor (toRealFloat n)))
        _ -> throwError FloorExpectedOneNumber
floor _ = throwError WrongNumberOfArguments
