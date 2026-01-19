module Glue.Lib.Bool.Not where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

not_ :: IR Eval
not_ = NativeFunc notImpl

notImpl :: [IR Eval] -> Eval (IR Eval)
notImpl [arg] = do
    val <- eval arg
    case val of
        Bool False -> pure $ Bool True
        _ -> pure $ Bool False
notImpl _ = throwError $ wrongArgumentType ["arg"]
