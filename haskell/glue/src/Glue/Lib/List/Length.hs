module Glue.Lib.List.Length where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

length :: [IR Eval] -> Eval (IR Eval)
length [arg] = do
    val <- evalRequired arg
    case val of
        List xs -> pure $ Integer (fromIntegral $ Prelude.length xs)
        _ -> throwError $ wrongArgumentType ["list"]
length _ = throwError wrongNumberOfArguments
