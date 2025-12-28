module Reactor.Lib.Math.Utility.Trunc where

import Data.Scientific (toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

trunc :: [IR Eval] -> Eval (IR Eval)
trunc [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromIntegral @Int (Prelude.truncate @Double (toRealFloat n)))
        _ -> throwError $ WrongArgumentType ["number"]
trunc _ = throwError WrongNumberOfArguments
