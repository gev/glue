module Glue.Lib.Bool.If where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..), isTruthy)

if_ :: IR Eval
if_ = Special ifImpl

ifImpl :: [IR Eval] -> Eval (IR Eval)
ifImpl [cond, thenExpr] = do
    condVal <- eval cond
    if isTruthy condVal then eval thenExpr else pure Void
ifImpl [cond, thenExpr, elseExpr] = do
    condVal <- eval cond
    if isTruthy condVal then eval thenExpr else eval elseExpr
ifImpl _ = throwError $ wrongArgumentType ["condition", "then", "else"]
