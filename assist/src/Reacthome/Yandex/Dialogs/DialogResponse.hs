module Reacthome.Yandex.Dialogs.DialogResponse where

import Data.Aeson
import Data.Text
import GHC.Generics
import Reacthome.Yandex.Dialogs.Response

data DialogResponse = DialogResponse
    { response :: Response
    , version :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)
