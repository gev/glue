module Reacthome.Yandex.Dialogs.Meta where

import Data.Aeson
import Data.Text
import GHC.Generics
import Reacthome.Yandex.Dialogs.Interfaces

data Meta = Meta
    { locale :: Text
    , timezone :: Text
    , client_id :: Text
    , interfaces :: Interfaces
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
