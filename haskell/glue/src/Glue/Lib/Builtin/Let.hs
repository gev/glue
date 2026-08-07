module Glue.Lib.Builtin.Let where

import Glue.Eval (Eval, eval, getEnv, putEnv, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

let' :: IR Eval
let' = Special letImpl

letImpl :: [IR Eval] -> Eval (IR Eval)
letImpl [] = throwError $ wrongArgumentType ["body"]
letImpl body = do
    -- Push new frame with bindings onto current environment
    currentEnv <- getEnv
    -- Evaluate body in extended environment
    result <- mapM eval body
    -- Pop the frame
    putEnv currentEnv
    pure . last $ result
