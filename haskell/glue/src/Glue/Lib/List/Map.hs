module Glue.Lib.List.Map where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

map :: IR Eval
map = NativeFunc mapImpl

mapImpl :: IR Eval -> Eval (IR Eval)
mapImpl funcIR = pure $ NativeFunc (mapOver funcIR)

mapOver :: IR Eval -> IR Eval -> Eval (IR Eval)
mapOver funcIR listIR = do
    func <- eval funcIR
    list <- eval listIR
    case list of
        List xs -> do
            -- Apply the function to each element by evaluating a list [func, x]
            results <- mapM (\x -> eval (List [func, x])) xs
            pure $ List results
        _ -> throwError $ wrongArgumentType ["function", "list"]
