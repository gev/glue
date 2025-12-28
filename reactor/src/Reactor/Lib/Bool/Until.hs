module Reactor.Lib.Bool.Until where

import Reactor.Eval (Eval, eval, evalRequired, throwError)
import Reactor.IR (IR (..))
import Reactor.Lib.Bool.Error (BoolError (..))

until_ :: [IR Eval] -> Eval (Maybe (IR Eval))
until_ (cond : body) = loop
  where
    loop = do
        case body of
            [] -> do
                condVal <- evalRequired cond
                case condVal of
                    Symbol "false" -> loop
                    _ -> pure Nothing
            _ -> do
                _ <- mapM eval body
                condVal <- evalRequired cond
                case condVal of
                    Symbol "false" -> loop
                    _ -> pure Nothing
until_ _ = throwError UntilExpectedAtLeastOneArg
