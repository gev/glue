module Glue.Lib.Bool.Until where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

until_ :: [IR Eval] -> Eval (IR Eval)
until_ (cond : body) = loop
  where
    loop = do
        case body of
            [] -> do
                condVal <- eval cond
                case condVal of
                    Bool False -> loop
                    _ -> pure Void
            _ -> do
                mapM_ eval body
                condVal <- eval cond
                case condVal of
                    Bool False -> loop
                    _ -> pure Void
until_ _ = throwError $ wrongArgumentType ["condition", "body"]
