module Glue.Lib.List.Length where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

length :: [IR Eval] -> Eval (IR Eval)
length [arg] = do
    val <- evalRequired arg
    case val of
        List xs -> pure $ Number (fromIntegral $ Prelude.length xs)
        _ -> throwError $ WrongArgumentType ["list"]
length _ = throwError WrongNumberOfArguments
