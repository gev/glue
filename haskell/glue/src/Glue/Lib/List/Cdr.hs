module Glue.Lib.List.Cdr where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (wrongArgumentType, wrongNumberOfArguments)
import Glue.IR (IR (..))

cdr :: IR Eval
cdr = NativeFunc cdrImpl

cdrImpl :: IR Eval -> Eval (IR Eval)
cdrImpl arg = do
    val <- eval arg
    case val of
        List (_ : xs) -> pure $ List xs
        List [] -> throwError $ wrongArgumentType ["non-empty list"]
        _ -> throwError $ wrongArgumentType ["list"]
