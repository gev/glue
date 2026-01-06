module Glue.Lib.Bool.Until where

import Glue.Eval (Eval, eval, evalRequired, throwError)
import Glue.Eval.Exception (RuntimeException (..))
import Glue.IR (IR (..))

until_ :: [IR Eval] -> Eval (Maybe (IR Eval))
until_ (cond : body) = loop
  where
    loop = do
        case body of
            [] -> do
                condVal <- evalRequired cond
                case condVal of
                    Bool False -> loop
                    _ -> pure Nothing
            _ -> do
                _ <- mapM eval body
                condVal <- evalRequired cond
                case condVal of
                    Bool False -> loop
                    _ -> pure Nothing
until_ _ = throwError $ WrongArgumentType ["condition", "body"]
