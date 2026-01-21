module Glue.Lib.Bool.Le where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

le :: IR Eval
le = NativeFunc leImpl

leImpl :: IR Eval -> Eval (IR Eval)
leImpl a = pure $ NativeFunc (leRight a)

leRight :: IR Eval -> IR Eval -> Eval (IR Eval)
leRight a b = case (a, b) of
    (Integer na, Integer nb) -> pure . Bool $ na <= nb
    (Float na, Float nb) -> pure . Bool $ na <= nb
    (Integer na, Float nb) -> pure . Bool $ fromIntegral na <= nb
    (Float na, Integer nb) -> pure . Bool $ na <= fromIntegral nb
    _ -> throwError $ wrongArgumentType ["number", "number"]
