module Reactor.Lib.Bool.Le where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

le :: [IR Eval] -> Eval (IR Eval)
le [a, b] = do
    va <- evalRequired a
    vb <- evalRequired b
    case (va, vb) of
        (Number na, Number nb) -> pure $ if na <= nb then Symbol "true" else Symbol "false"
        _ -> throwError $ WrongArgumentType "le" ["number", "number"]
le _ = throwError $ WrongArgumentType "le" ["number", "number"]
