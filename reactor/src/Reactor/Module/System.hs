{- HLINT ignore "Eta reduce" -}
module Reactor.Module.System where

import Reactor.Eval (Eval)
import Reactor.IR (Frame)
import Reactor.Module.Import qualified as Import

-- | Standard library with module system support
libWithModules :: Frame Eval
libWithModules = Import.importFunc
