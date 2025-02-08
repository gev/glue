module Reacthome.Yandex.Dialogs.Screen where

import Data.Aeson
import GHC.Generics

data Screen = Screen
    {
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
