module Reactor.Lib.Math.Const where

import Data.Scientific (fromFloatDigits)
import Reactor.Env qualified as E
import Reactor.Eval (Eval)
import Reactor.IR (Frame, IR (..))

const :: Frame Eval
const =
    E.frameFromList
        [ ("pi", Number (fromFloatDigits @Double pi))
        , ("e", Number (fromFloatDigits @Double (exp 1)))
        ]
