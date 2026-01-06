module Glue.Lib.Bool.While where

import Glue.Eval (Eval, eval, evalRequired, throwError)
import Glue.Eval.Exception (RuntimeException (..))
import Glue.IR (IR (..))

while_ :: [IR Eval] -> Eval (Maybe (IR Eval))
while_ (cond : body) = loop
 where
  loop = do
    condVal <- evalRequired cond
    case condVal of
      Bool False -> pure Nothing
      _ -> case body of
        [] -> loop
        _ -> do
          _ <- mapM eval body
          loop
while_ _ = throwError $ WrongArgumentType ["condition", "body"]
