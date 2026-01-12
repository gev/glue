module Glue.Module (
    RegisteredModule (..),
    ImportedModule (..),
    ModuleInfo (..),
    nativeModule,
    envFromModule,
    envFromModules,
) where

import Data.Map.Strict (Map, keys)
import Data.Text (Text)
import Glue.Env (frameFromList, fromFrame, unionFrames)
import Glue.IR (Env, Frame, IR)
import Prelude hiding (mod)

-- | A registered module containing metadata and body for evaluation
data RegisteredModule m = RegisteredModule
    { name :: Text
    , exports :: [Text]
    , body :: [IR m]
    }

instance Show (RegisteredModule ir) where
    show mod = "Module {name = " <> show mod.name <> ", exports = " <> show mod.exports <> ", body = <" <> show (length mod.body) <> " forms>}"

instance Eq (RegisteredModule m) where
    m1 == m2 = name m1 == name m2 && m1.exports == m2.exports && m1.body == m2.body

-- | A cached imported module with evaluated exports and evaluation context
data ImportedModule m = ImportedModule
    { moduleName :: Text
    , exportedValues :: Map Text (IR m) -- Cached exports
    , evaluationRootEnv :: Env m -- Root env used for evaluation
    }

instance Show (ImportedModule m) where
    show im = "ImportedModule {moduleName = " <> show im.moduleName <> ", exports = " <> show (keys im.exportedValues) <> "}"

instance Eq (ImportedModule m) where
    im1 == im2 = im1.moduleName == im2.moduleName

-- | Result of parsing a module
data ModuleInfo m = ModuleInfo
    { moduleName :: Text
    , exports :: [Text]
    , definitions :: [(Text, IR m)]
    }
    deriving (Show, Eq)

nativeModule :: Text -> [(Text, IR m)] -> ModuleInfo m
nativeModule moduleName definitions =
    ModuleInfo
        { moduleName
        , exports = fst <$> definitions
        , definitions
        }

envFromModule :: ModuleInfo m -> Env m
envFromModule = fromFrame . frameFromModule

envFromModules :: [ModuleInfo m] -> Env m
envFromModules = fromFrame . unionFrames . fmap frameFromModule

frameFromModule :: ModuleInfo m -> Frame m
frameFromModule = frameFromList . definitions
