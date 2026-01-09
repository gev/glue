module Glue.Lib.Builtin.Def where

import Glue.Eval (Eval, defineVarEval, evalRequired, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

def :: [IR Eval] -> Eval (IR Eval)
def [Symbol name, rawVal] = do
    val <- evalRequired rawVal
    defineVarEval name val
    pure Void
def _ = throwError $ wrongArgumentType ["symbol", "value"]
