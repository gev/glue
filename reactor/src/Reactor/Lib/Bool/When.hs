module Reactor.Lib.Bool.When where

import Reactor.Eval (Eval, eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

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
when_ _ = throwError $ WrongArgumentType "when" ["condition", "body"]
