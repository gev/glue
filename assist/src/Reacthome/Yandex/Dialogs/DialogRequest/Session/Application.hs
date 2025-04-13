module Reacthome.Yandex.Dialogs.DialogRequest.Session.Application where

import Data.Aeson
import Data.Text
import GHC.Generics

newtype Application = Application
    { application_id :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
