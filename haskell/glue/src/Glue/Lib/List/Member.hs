module Glue.Lib.List.Member where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

member :: IR Eval
member = NativeFunc memberImpl

memberImpl :: IR Eval -> Eval (IR Eval)
memberImpl itemIR = pure $ NativeFunc (memberIn itemIR)

memberIn :: IR Eval -> IR Eval -> Eval (IR Eval)
memberIn item list = case list of
    List xs -> pure . Bool $ item `elem` xs
    _ -> throwError $ wrongArgumentType ["list"]
