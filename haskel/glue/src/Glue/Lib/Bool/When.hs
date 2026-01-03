module Glue.Lib.Bool.When where

import Glue.Eval (Eval, eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

when_ :: [IR Eval] -> Eval (Maybe (IR Eval))
when_ (cond : body) = do
    condVal <- evalRequired cond
    case condVal of
        Symbol "false" -> pure Nothing
        _ -> case body of
            [] -> pure Nothing
            _ -> do
                results <- mapM eval body
                pure $ last results
when_ _ = throwError $ WrongArgumentType ["condition", "body"]
