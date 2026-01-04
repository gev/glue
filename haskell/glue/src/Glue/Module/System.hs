{- HLINT ignore "Eta reduce" -}
module Glue.Module.System where

import Glue.Eval (Eval)
import Glue.IR (Frame)
import Glue.Module.Import qualified as Import

-- | Standard library with module system support
libWithModules :: Frame Eval
libWithModules = Import.importFunc
