module Glue.Lib.List.Foldr where

import Glue.Eval (Eval, apply, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

{- | Strict right-to-left fold over a list.

Curried signature: @foldr func initVal list@

Arguments:
  * @func@: The accumulator function applied at each step.
  * @initVal@: The initial value of the accumulator.
  * @list@: The target @List@ to fold over.

Lambda argument order: @(x, acc)@
  * @x@: The current element of the list (first parameter).
  * @acc@: The current accumulator value (second parameter).
-}
foldr :: IR Eval
foldr = NativeFunc foldrImpl

foldrImpl :: IR Eval -> Eval (IR Eval)
foldrImpl func = pure $ NativeFunc (foldrInit func)

foldrInit :: IR Eval -> IR Eval -> Eval (IR Eval)
foldrInit func initVal = pure $ NativeFunc (foldrOver func initVal)

foldrOver :: IR Eval -> IR Eval -> IR Eval -> Eval (IR Eval)
foldrOver func initVal list = case list of
    List xs -> do
        -- Strict right-to-left fold (foldr') using Prelude.foldr with strict binding
        Prelude.foldr
            ( \x next acc -> do
                acc' <- apply func [x, acc]
                next acc'
            )
            pure
            xs
            initVal
    _ -> throwError $ wrongArgumentType ["function", "any", "list"]
