module Glue.Lib.List.Length where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

length :: IR Eval
length = NativeFunc lengthImpl

lengthImpl :: IR Eval -> Eval (IR Eval)
lengthImpl arg = case arg of
    List xs -> pure $ Integer (fromIntegral $ Prelude.length xs)
    _ -> throwError $ wrongArgumentType ["list"]
