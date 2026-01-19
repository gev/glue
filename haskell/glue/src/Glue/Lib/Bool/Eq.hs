module Glue.Lib.Bool.Eq where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

eq :: IR Eval
eq = NativeFunc eqImpl

eqImpl :: IR Eval -> Eval (IR Eval)
eqImpl a = pure $ NativeFunc (eqRight a)

eqRight :: IR Eval -> IR Eval -> Eval (IR Eval)
eqRight a b = do
    va <- eval a
    vb <- eval b
    pure . Bool $ va == vb
