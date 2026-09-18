module Glue.Lib.Builtin.Concat where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

concat :: IR Eval
concat = NativeFunc concatImpl

concatImpl :: IR Eval -> Eval (IR Eval)
concatImpl ir1 = pure $ NativeFunc (concatTo ir1)

concatTo :: IR Eval -> IR Eval -> Eval (IR Eval)
concatTo ir1 ir2 = case (ir1, ir2) of
    (Object xs, Object ys) -> pure $ Object (xs <> ys)
    (String xs, String ys) -> pure $ String (xs <> ys)
    (List xs, List ys) -> pure $ List (xs <> ys)
    _ -> throwError $ wrongArgumentType ["Lists, objects or strings required"]
