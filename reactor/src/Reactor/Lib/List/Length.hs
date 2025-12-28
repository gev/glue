module Reactor.Lib.List.Length where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

length :: [IR Eval] -> Eval (IR Eval)
length [arg] = do
    val <- evalRequired arg
    case val of
        List xs -> pure $ Number (fromIntegral $ Prelude.length xs)
        _ -> throwError $ WrongArgumentType ["list"]
length _ = throwError WrongNumberOfArguments
