{- HLINT ignore "Eta reduce" -}
module Reactor.Module.System where

import Reactor.Eval (Eval)
import Reactor.IR (Frame)
import Reactor.Module.Import qualified as Import
import Reactor.Module.Registration (RegistryRef)

-- | Standard library with module system support
libWithModules :: RegistryRef Eval -> Frame Eval
libWithModules registry = Import.importFunc registry
