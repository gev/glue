module Glue.Lib.List.Cons where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

cons :: IR Eval
cons = NativeFunc consImpl

consImpl :: IR Eval -> Eval (IR Eval)
consImpl headArg = pure $ NativeFunc (consWith headArg)

consWith :: IR Eval -> IR Eval -> Eval (IR Eval)
consWith headArg tailArg = do
    headVal <- eval headArg
    tailVal <- eval tailArg
    case tailVal of
        List xs -> pure $ List (headVal : xs)
        _ -> throwError $ wrongArgumentType ["list"]
