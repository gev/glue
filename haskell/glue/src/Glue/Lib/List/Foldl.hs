module Glue.Lib.List.Foldl where

import Control.Monad (foldM)
import Glue.Eval (Eval, apply, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

foldl :: IR Eval
foldl = NativeFunc foldlImpl

foldlImpl :: IR Eval -> Eval (IR Eval)
foldlImpl func = pure $ NativeFunc (foldlInit func)

foldlInit :: IR Eval -> IR Eval -> Eval (IR Eval)
foldlInit func initVal = pure $ NativeFunc (foldlOver func initVal)

foldlOver :: IR Eval -> IR Eval -> IR Eval -> Eval (IR Eval)
foldlOver func initVal list = case list of
    List xs -> do
        -- Strict left-to-right fold accumulating effects/values via foldM
        foldM (\acc x -> apply func [acc, x]) initVal xs
    _ -> throwError $ wrongArgumentType ["function", "any", "list"]
