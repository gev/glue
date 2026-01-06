module Glue.Lib.List.Map where

import Glue.Eval (Eval, eval, evalRequired, throwError)
import Glue.Eval.Exception (RuntimeException (..))
import Glue.IR (IR (..))

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
