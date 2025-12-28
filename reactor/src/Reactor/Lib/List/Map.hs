module Reactor.Lib.List.Map where

import Reactor.Eval (Eval, eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

map :: [IR Eval] -> Eval (IR Eval)
map [funcIR, listIR] = do
    func <- evalRequired funcIR
    list <- evalRequired listIR
    case list of
        List xs -> do
            -- Apply the function to each element by evaluating a list [func, x]
            results <- mapM (\x -> eval (List [func, x]) >>= maybe (throwError ExpectedValue) pure) xs
            pure $ List results
        _ -> throwError $ WrongArgumentType ["function", "list"]
map _ = throwError WrongNumberOfArguments
