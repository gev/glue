module Reactor.Lib.Math.Utility.Floor where

import Data.Scientific (toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

floor :: [IR Eval] -> Eval (IR Eval)
floor [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromIntegral @Int (Prelude.floor @Double (toRealFloat n)))
        _ -> throwError $ WrongArgumentType ["number"]
floor _ = throwError WrongNumberOfArguments
