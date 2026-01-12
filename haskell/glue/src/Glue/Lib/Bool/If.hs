module Glue.Lib.Bool.If where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

if_ :: [IR Eval] -> Eval (IR Eval)
if_ [cond, thenExpr, elseExpr] = do
    condVal <- eval cond
    case condVal of
        Bool False -> eval elseExpr
        _ -> eval thenExpr
if_ _ = throwError $ wrongArgumentType ["condition", "then", "else"]
