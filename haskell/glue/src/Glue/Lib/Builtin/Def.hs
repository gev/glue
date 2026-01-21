module Glue.Lib.Builtin.Def where

import Glue.Eval (Eval, defineVarEval, eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))
import Glue.Lib.Builtin.Lambda (extractSymbols, makeClosure)

def :: IR Eval
def = Special defImpl

defImpl :: [IR Eval] -> Eval (IR Eval)
defImpl [Symbol name, rawVal] = do
    val <- eval rawVal
    defineVarEval name val
    pure Void
defImpl (List (Symbol name : params) : body) = do
    paramSymbols <- case extractSymbols params of
        Right ps -> pure ps
        Left _ -> throwError $ wrongArgumentType ["symbols in function parameters"]
    let bodyExpr = case body of
            [] -> Void
            [single] -> single
            multiple -> List multiple
    closure <- makeClosure paramSymbols bodyExpr

    defineVarEval name closure
    pure closure
defImpl _ = throwError $ wrongArgumentType ["symbol", "value"]
