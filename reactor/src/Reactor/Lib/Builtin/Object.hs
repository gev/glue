module Reactor.Lib.Builtin.Object where

import Data.Map.Strict qualified as Map
import Reactor.Eval (Eval, evalRequired)
import Reactor.IR (IR (..))

object :: [IR Eval] -> Eval (Maybe (IR Eval))
object = \case
    [Object m] -> do
        evaluatedMap <- traverse evalRequired m
        pure $ Just $ Object evaluatedMap
    [] -> pure $ Just $ Object Map.empty
    _ -> error "object builtin: unexpected arguments"
