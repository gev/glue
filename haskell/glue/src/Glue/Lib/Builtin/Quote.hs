module Glue.Lib.Builtin.Quote (quote) where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

{- | Quote special form - returns its argument unevaluated
'quote' prevents evaluation of its argument, returning it as-is
-}
quote :: IR Eval
quote = Special quoteImpl

quoteImpl :: [IR Eval] -> Eval (IR Eval)
quoteImpl [x] = pure x
quoteImpl _ = throwError $ wrongArgumentType ["single argument `quote`"]
