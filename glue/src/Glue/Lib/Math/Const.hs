module Glue.Lib.Math.Const where

import Data.Scientific (fromFloatDigits)
import Glue.Env qualified as E
import Glue.Eval (Eval)
import Glue.IR (Frame, IR (..))

const :: Frame Eval
const =
    E.frameFromList
        [ ("pi", Number (fromFloatDigits @Double pi))
        , ("e", Number (fromFloatDigits @Double (exp 1)))
        ]
