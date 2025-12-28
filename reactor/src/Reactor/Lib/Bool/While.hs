module Reactor.Lib.Bool.While where

import Reactor.Eval (Eval, eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

while_ :: [IR Eval] -> Eval (Maybe (IR Eval))
while_ (cond : body) = loop
 where
  loop = do
    condVal <- evalRequired cond
    case condVal of
      Symbol "false" -> pure Nothing
      _ -> case body of
        [] -> loop
        _ -> do
          _ <- mapM eval body
          loop
while_ _ = throwError $ WrongArgumentType ["condition", "body"]
