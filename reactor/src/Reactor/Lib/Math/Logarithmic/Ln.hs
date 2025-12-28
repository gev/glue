module Reactor.Lib.Math.Logarithmic.Ln where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

ln :: [IR Eval] -> Eval (IR Eval)
ln [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> do
            let realVal = toRealFloat @Double n
            if realVal <= 0
                then throwError $ WrongArgumentType ["positive number"]
                else pure $ Number (fromFloatDigits (Prelude.log realVal))
        _ -> throwError $ WrongArgumentType ["number"]
ln _ = throwError $ WrongArgumentType ["number"]
