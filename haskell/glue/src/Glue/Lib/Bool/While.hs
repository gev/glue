module Glue.Lib.Bool.While where

import Glue.Eval (Eval, eval, evalRequired, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

while_ :: [IR Eval] -> Eval (IR Eval)
while_ (cond : body) = loop
 where
  loop = do
    condVal <- evalRequired cond
    case condVal of
      Bool False -> pure Void
      _ -> case body of
        [] -> loop
        _ -> do
          mapM_ eval body
          loop
while_ _ = throwError $ wrongArgumentType ["condition", "body"]
