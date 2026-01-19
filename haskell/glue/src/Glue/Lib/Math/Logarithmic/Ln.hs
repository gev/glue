module Glue.Lib.Math.Logarithmic.Ln where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

-- Natural logarithm function (ln)
-- Mirrors Haskell Glue.Lib.Math.Logarithmic.Ln.ln exactly
ln :: IR Eval
ln = NativeFunc lnImpl

-- Natural logarithm function implementation
-- Mirrors Haskell Glue.Lib.Math.Logarithmic.Ln.lnImpl exactly
lnImpl :: IR Eval -> Eval (IR Eval)
lnImpl arg = do
    va <- eval arg
    case va of
        Integer n -> pure $ Float (Prelude.log (fromIntegral n))
        Float n -> pure $ Float (Prelude.log n)
        _ -> throwError $ wrongArgumentType ["number"]
