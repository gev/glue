module Glue.Lib.Bool.While where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

while_ :: IR Eval
while_ = Special whileImpl

whileImpl :: [IR Eval] -> Eval (IR Eval)
whileImpl (cond : body) = loop
 where
  loop = do
    condVal <- eval cond
    case condVal of
      Bool False -> pure Void
      _ -> case body of
        [] -> loop
        _ -> do
          mapM_ eval body
          loop
whileImpl _ = throwError $ wrongArgumentType ["condition", "body"]
