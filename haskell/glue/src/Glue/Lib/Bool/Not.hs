module Glue.Lib.Bool.Not where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

-- Implementation function (current logic)
notImpl :: [IR Eval] -> Eval (IR Eval)
notImpl [arg] = do
    val <- eval arg
    case val of
        Bool False -> pure $ Bool True
        _ -> pure $ Bool False
notImpl _ = throwError $ wrongArgumentType ["arg"]

-- Function definition with NativeFunc constructor
not_ :: IR Eval
not_ = NativeFunc notImpl
