module Glue.Lib.Bool.If where

import Glue.Eval (Eval, eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

if_ :: [IR Eval] -> Eval (Maybe (IR Eval))
if_ [cond, thenExpr, elseExpr] = do
    condVal <- evalRequired cond
    case condVal of
        Symbol "false" -> eval elseExpr
        _ -> eval thenExpr
if_ _ = throwError $ WrongArgumentType ["condition", "then", "else"]
