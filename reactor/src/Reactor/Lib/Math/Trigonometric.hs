module Reactor.Lib.Math.Trigonometric where

import Reactor.Env qualified as E
import Reactor.Eval (Eval)
import Reactor.IR (Frame, IR (..), Native (..))
import Reactor.Lib.Math.Trigonometric.Acos qualified as Acos
import Reactor.Lib.Math.Trigonometric.Asin qualified as Asin
import Reactor.Lib.Math.Trigonometric.Atan qualified as Atan
import Reactor.Lib.Math.Trigonometric.Cos qualified as Cos
import Reactor.Lib.Math.Trigonometric.Sin qualified as Sin
import Reactor.Lib.Math.Trigonometric.Tan qualified as Tan

trigonometric :: Frame Eval
trigonometric = E.frameFromList [("sin", Native (Func Sin.sin)), ("cos", Native (Func Cos.cos))]

-- Note: tan, asin, acos, atan are not yet integrated
-- tanFrame = E.singletonFrame "tan" Tan.tan
-- asinFrame = E.singletonFrame "asin" Asin.asin
-- acosFrame = E.singletonFrame "acos" Acos.acos
-- atanFrame = E.singletonFrame "atan" Atan.atan
