module Glue.Lib.Bool.Until where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

until_ :: IR Eval
until_ = Special untilImpl

untilImpl :: [IR Eval] -> Eval (IR Eval)
untilImpl (cond : body) = loop
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
untilImpl _ = throwError $ wrongArgumentType ["condition", "body"]
