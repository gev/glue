module Reactor.Lib.Builtin.Object where

import Data.Map.Strict qualified as Map
import Reactor.Eval (Eval)
import Reactor.IR (IR (..))

object :: [IR Eval] -> Eval (IR Eval)
object = \case
    [obj@(Object _)] -> pure obj
    [] -> pure $ Object Map.empty
    _ -> error "object builtin: unexpected arguments"
