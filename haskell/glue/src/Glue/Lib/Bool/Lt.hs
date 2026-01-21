module Glue.Lib.Bool.Lt where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

lt :: IR Eval
lt = NativeFunc ltImpl

ltImpl :: IR Eval -> Eval (IR Eval)
ltImpl a = pure $ NativeFunc (ltRight a)

ltRight :: IR Eval -> IR Eval -> Eval (IR Eval)
ltRight a b = case (a, b) of
    (Integer na, Integer nb) -> pure . Bool $ na < nb
    (Float na, Float nb) -> pure . Bool $ na < nb
    (Integer na, Float nb) -> pure . Bool $ fromIntegral na < nb
    (Float na, Integer nb) -> pure . Bool $ na < fromIntegral nb
    _ -> throwError $ wrongArgumentType ["number", "number"]
