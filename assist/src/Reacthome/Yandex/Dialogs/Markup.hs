module Reacthome.Yandex.Dialogs.Markup where

import Data.Aeson
import GHC.Generics

newtype Markup = Markup
    { dangerous_context :: Bool
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
