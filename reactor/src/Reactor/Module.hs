module Reactor.Module (
    RegisteredModule (..),
    ImportedModule (..),
) where

import Data.Map.Strict (Map, keys)
import Data.Text (Text)
import Reactor.IR (Env, IR)
import Prelude hiding (mod)

-- | A registered module containing metadata and body for evaluation
data RegisteredModule m = RegisteredModule
    { name :: Text
    , exports :: [Text]
    , body :: [IR m] -- Generic IR type
    }

instance Show (RegisteredModule ir) where
    show mod = "Module {name = " <> show (name mod) <> ", exports = " <> show (exports mod) <> ", body = <" <> show (length (body mod)) <> " forms>}"

instance Eq (RegisteredModule m) where
    m1 == m2 = name m1 == name m2 && exports m1 == exports m2 && body m1 == body m2

-- | A cached imported module with evaluated exports and evaluation context
data ImportedModule m = ImportedModule
    { moduleName :: Text
    , exportedValues :: Map Text (IR m) -- Cached exports
    , evaluationRootEnv :: Env m -- Root env used for evaluation
    }

instance Show (ImportedModule m) where
    show im = "ImportedModule {moduleName = " <> show (moduleName im) <> ", exports = " <> show (keys (exportedValues im)) <> "}"

instance Eq (ImportedModule m) where
    im1 == im2 = moduleName im1 == moduleName im2
