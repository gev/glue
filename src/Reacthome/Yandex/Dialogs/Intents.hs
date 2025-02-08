module Reacthome.Yandex.Dialogs.Intents where

import Data.Aeson
import GHC.Generics

data Intents = Intents
    {}
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
