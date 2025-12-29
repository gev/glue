module Reactor.Module where

import Data.Map.Strict (Map, keys)
import Data.Text (Text)
import Reactor.IR (Env, IR)

-- | A registered module containing metadata and body for evaluation
data Module m = Module
    { name :: Text
    , exports :: [Text]
    , body :: [IR m]
    }

instance Show (Module m) where
    show mod = "Module {name = " <> show (name mod) <> ", exports = " <> show (exports mod) <> ", body = <" <> show (length (body mod)) <> " forms>}"

instance Eq (Module m) where
    m1 == m2 = name m1 == name m2 && exports m1 == exports m2 && body m1 == body m2

-- | Global registry of registered modules
type ModuleRegistry m = Map Text (Module m)

-- | A cached imported module with evaluated exports and evaluation context
data ImportedModule m = ImportedModule
    { moduleName :: Text
    , exportedValues :: Map Text (IR m) -- Cached exports
    , evaluationRootEnv :: Env m -- Root env used for evaluation
    }

instance Show (ImportedModule m) where
    show im = "ImportedModule {moduleName = " <> show (moduleName im) <> ", exports = " <> show (keys (exportedValues im)) <> "}"

-- | Global cache of imported modules (evaluated results)
type ImportedModuleCache m = Map Text (ImportedModule m)
