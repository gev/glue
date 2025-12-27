module Reactor.Lib.Builtin.Quote where

import Reactor.Eval (Eval, throwError)
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR (IR (..))

quote :: [IR Eval] -> Eval (Maybe (IR Eval))
quote args = case makeQuote args of
    Right val -> pure (Just val)
    Left err -> throwError err

makeQuote :: [IR m] -> Either EvalError (IR m)
makeQuote [v] = Right v
makeQuote _ = Left QuoteExpectedExactlyOneArgument
