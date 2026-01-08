module Glue.Lib.Math.Const where

import Data.Scientific (fromFloatDigits)
import Glue.Env qualified as E
import Glue.Eval (Eval)
import Glue.IR (Frame, IR (..))

const :: Frame Eval
const =
    E.frameFromList
        [ ("pi", Float pi)
        , ("e", Float (exp 1))
        ]
