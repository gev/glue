module Reactor.Module.Error where

import Data.Text (Text)
import Reactor.Eval (Eval)
import Reactor.IR (IR)

-- | Errors during module parsing and registration
data ModuleRegistryError
    = InvalidModuleStructure Text
    | InvalidExportList [IR Eval]
    | DuplicateModuleName Text
    deriving (Show, Eq)
