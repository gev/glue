module Reacthome.Yandex.Dialogs.DialogRequest.Request.Markup where

import Data.Aeson
import GHC.Generics

newtype Markup = Markup
    { dangerous_context :: Bool
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
