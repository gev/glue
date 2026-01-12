module Glue.Lib.Bool.When where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

when_ :: [IR Eval] -> Eval (IR Eval)
when_ (cond : body) = do
    condVal <- eval cond
    case condVal of
        Bool False -> pure Void
        _ -> case body of
            [] -> pure Void
            _ -> do
                results <- mapM eval body
                pure $ last results
when_ _ = throwError $ wrongArgumentType ["condition", "body"]
