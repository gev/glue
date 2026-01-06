module Glue.Lib.Bool.When where

import Glue.Eval (Eval, eval, evalRequired, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

when_ :: [IR Eval] -> Eval (Maybe (IR Eval))
when_ (cond : body) = do
    condVal <- evalRequired cond
    case condVal of
        Bool False -> pure Nothing
        _ -> case body of
            [] -> pure Nothing
            _ -> do
                results <- mapM eval body
                pure $ last results
when_ _ = throwError $ wrongArgumentType ["condition", "body"]
