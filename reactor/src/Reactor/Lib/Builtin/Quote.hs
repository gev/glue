module Reactor.Lib.Builtin.Quote where

import Reactor.Eval (Eval, throwError)
import Reactor.IR (IR (..))
import Reactor.Lib.Builtin.Error (BuiltinError (..))

quote :: [IR Eval] -> Eval (Maybe (IR Eval))
quote args = case makeQuote args of
    Right val -> pure (Just val)
    Left err -> throwError err

makeQuote :: [IR m] -> Either BuiltinError (IR m)
makeQuote [v] = Right v
makeQuote _ = Left QuoteExpectedExactlyOneArgument
