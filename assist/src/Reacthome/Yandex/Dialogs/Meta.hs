module Reacthome.Yandex.Dialogs.Meta where

import Data.Aeson
import Data.Text
import GHC.Generics

data Meta = Meta
    { locale :: Text
    , timezone :: Text
    , client_id :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
