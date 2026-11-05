module Glue.Lib.Builtin.Do where

import Glue.Eval (Eval, eval, getEnv, putEnv, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

do' :: IR Eval
do' = Special doImpl

doImpl :: [IR Eval] -> Eval (IR Eval)
doImpl [] = throwError $ wrongArgumentType ["body"]
doImpl body = do
    -- Push new frame with bindings onto current environment
    currentEnv <- getEnv
    -- Evaluate body in extended environment
    result <- mapM eval body
    -- Pop the frame
    putEnv currentEnv
    pure . last $ result
