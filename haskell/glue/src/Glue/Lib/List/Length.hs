module Glue.Lib.List.Length where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

length :: IR Eval
length = NativeFunc lengthImpl

lengthImpl :: IR Eval -> Eval (IR Eval)
lengthImpl arg = do
    val <- eval arg
    case val of
        List xs -> pure $ Integer (fromIntegral $ Prelude.length xs)
        _ -> throwError $ wrongArgumentType ["list"]
