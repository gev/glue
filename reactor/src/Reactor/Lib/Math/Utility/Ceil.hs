module Reactor.Lib.Math.Utility.Ceil where

import Data.Scientific (toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR (IR (..))

ceil :: [IR Eval] -> Eval (IR Eval)
ceil [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromIntegral @Int (Prelude.ceiling @Double (toRealFloat n)))
        _ -> throwError CeilExpectedOneNumber
ceil _ = throwError WrongNumberOfArguments
