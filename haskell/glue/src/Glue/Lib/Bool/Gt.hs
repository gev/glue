module Glue.Lib.Bool.Gt where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

gt :: IR Eval
gt = NativeFunc gtImpl

gtImpl :: IR Eval -> Eval (IR Eval)
gtImpl a = pure $ NativeFunc (gtRight a)

gtRight :: IR Eval -> IR Eval -> Eval (IR Eval)
gtRight a b = case (a, b) of
    (Integer na, Integer nb) -> pure . Bool $ na > nb
    (Float na, Float nb) -> pure . Bool $ na > nb
    (Integer na, Float nb) -> pure . Bool $ fromIntegral na > nb
    (Float na, Integer nb) -> pure . Bool $ na > fromIntegral nb
    _ -> throwError $ wrongArgumentType ["number", "number"]
