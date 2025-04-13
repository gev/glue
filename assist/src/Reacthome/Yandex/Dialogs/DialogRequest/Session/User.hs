module Reacthome.Yandex.Dialogs.DialogRequest.Session.User where

import Data.Aeson
import Data.Text
import GHC.Generics

newtype User = User
    { user_id :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
