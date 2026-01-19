module Glue.Lib.Bool.Ge where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

ge :: IR Eval
ge = NativeFunc geImpl

geImpl :: IR Eval -> Eval (IR Eval)
geImpl a = pure $ NativeFunc (geRight a)

geRight :: IR Eval -> IR Eval -> Eval (IR Eval)
geRight a b = do
    va <- eval a
    vb <- eval b
    case (va, vb) of
        (Integer na, Integer nb) -> pure . Bool $ na >= nb
        (Float na, Float nb) -> pure . Bool $ na >= nb
        (Integer na, Float nb) -> pure . Bool $ fromIntegral na >= nb
        (Float na, Integer nb) -> pure . Bool $ na >= fromIntegral nb
        _ -> throwError $ wrongArgumentType ["number", "number"]
