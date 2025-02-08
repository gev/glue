module Reacthome.Yandex.Dialogs.Entity where

import Data.Aeson
import GHC.Generics

data Entity = Entity
    {}
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
