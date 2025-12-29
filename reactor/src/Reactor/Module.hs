module Reactor.Module where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Reactor.IR (IR)

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
