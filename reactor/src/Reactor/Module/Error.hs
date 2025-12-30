module Reactor.Module.Error where

import Data.Text (Text)
import Reactor.IR (IR)

-- | Errors during module parsing and registration
data ModuleRegistryError m
    = InvalidModuleStructure Text
    | InvalidExportList [IR m]
    | DuplicateModuleName Text
    deriving (Show, Eq)
