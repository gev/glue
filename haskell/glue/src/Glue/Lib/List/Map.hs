module Glue.Lib.List.Map where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

map :: [IR Eval] -> Eval (IR Eval)
map [funcIR, listIR] = do
    func <- eval funcIR
    list <- eval listIR
    case list of
        List xs -> do
            -- Apply the function to each element by evaluating a list [func, x]
            results <- mapM (\x -> eval (List [func, x])) xs
            pure $ List results
        _ -> throwError $ wrongArgumentType ["function", "list"]
map _ = throwError wrongNumberOfArguments
