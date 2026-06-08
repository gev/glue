module Glue.Runtime where

import Data.Text (Text)
import Glue.IR (Env)
import Glue.Module.Cache (ImportedModuleCache)
import Glue.Module.Registry (ModuleRegistry)

type CallStack = [Text]

data Runtime m = Runtime
    { env :: Env m
    , stack :: CallStack
    , registry :: ModuleRegistry m
    , importCache :: ImportedModuleCache m
    , rootEnv :: Env m
    }
    deriving (Show, Eq)
