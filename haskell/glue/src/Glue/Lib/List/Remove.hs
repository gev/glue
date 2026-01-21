module Glue.Lib.List.Remove where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

remove :: IR Eval
remove = NativeFunc removeImpl

removeImpl :: IR Eval -> Eval (IR Eval)
removeImpl itemIR = pure $ NativeFunc (removeFrom itemIR)

removeFrom :: IR Eval -> IR Eval -> Eval (IR Eval)
removeFrom item list = case list of
    List xs -> do
        let filtered = filter (/= item) xs
        pure $ List filtered
    _ -> throwError $ wrongArgumentType ["list"]
