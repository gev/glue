module Glue.Lib.Builtin.Let where

import Data.Map.Strict qualified as Map
import Glue.Eval (Eval, eval, evalRequired, getEnv, putEnv, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

let' :: [IR Eval] -> Eval (Maybe (IR Eval))
let' [Object bindingsMap, body] = do
    -- bindingsMap already has keys without : prefix
    evaluatedPairs <- mapM evalBinding (Map.toList bindingsMap)

    -- Push new frame with bindings onto current environment
    currentEnv <- getEnv
    let newFrame = Map.fromList evaluatedPairs
    putEnv (newFrame : currentEnv)

    -- Evaluate body in extended environment
    result <- eval body

    -- Pop the frame
    putEnv currentEnv

    pure result
  where
    evalBinding (name, rawVal) = do
        val <- evalRequired rawVal
        pure (name, val)
let' _ = throwError $ wrongArgumentType ["object", "body"]
