module Reactor.Eval.Error where

import Data.Text (Text)

newtype EvalError = EvalError Text
    deriving (Show, Eq)
