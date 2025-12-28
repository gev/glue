module Reactor.Lib.Bool.If where

import Reactor.Eval (Eval, eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

if_ :: [IR Eval] -> Eval (Maybe (IR Eval))
if_ [cond, thenExpr, elseExpr] = do
    condVal <- evalRequired cond
    case condVal of
        Symbol "false" -> eval elseExpr
        _ -> eval thenExpr
if_ _ = throwError $ WrongArgumentType "if" ["condition", "then", "else"]
