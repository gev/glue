module Reacthome.Yandex.Dialogs.DialogRequest where

import Data.Aeson
import Data.Text
import GHC.Generics (Generic)
import Reacthome.Yandex.Dialogs.Meta
import Reacthome.Yandex.Dialogs.Request
import Reacthome.Yandex.Dialogs.Session

data DialogRequest = DialogRequest
    { meta :: Meta
    , session :: Session
    , request :: Request
    , version :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
