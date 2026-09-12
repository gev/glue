module Glue.Lib.Bool.Maybe where

import Glue.Eval (Eval, apply, eval, throwError)
import Glue.Eval.Exception (wrongNumberOfArguments)
import Glue.IR (IR (..), exists)

maybe_ :: IR Eval
maybe_ = Special maybeImpl

maybeImpl :: [IR Eval] -> Eval (IR Eval)
maybeImpl [f, x] = do
    x' <- eval x
    if exists x'
        then do
            g <- eval f
            apply g [x']
        else pure Void
maybeImpl _ = throwError wrongNumberOfArguments
