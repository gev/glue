module Reactor.Module.System where

import Reactor.Env qualified as E
import Reactor.Eval (Eval)
import Reactor.IR (Frame)
import Reactor.Lib qualified as Lib
import Reactor.Module.Import qualified as Import
import Reactor.Module.Registration (RegistryRef)

-- | Standard library with module system support
libWithModules :: RegistryRef Eval -> Frame Eval
libWithModules registry = E.unionFrames Lib.lib (Import.importFunc registry)
