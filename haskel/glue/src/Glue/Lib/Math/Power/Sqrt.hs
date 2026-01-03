module Glue.Lib.Math.Power.Sqrt where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

sqrt :: [IR Eval] -> Eval (IR Eval)
sqrt [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> do
            let realVal = toRealFloat @Double n
            if realVal < 0
                then throwError $ WrongArgumentType ["number"]
                else pure $ Number (fromFloatDigits (Prelude.sqrt realVal))
        _ -> throwError $ WrongArgumentType ["number"]
sqrt _ = throwError WrongNumberOfArguments
