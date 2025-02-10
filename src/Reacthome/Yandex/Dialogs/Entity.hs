module Reacthome.Yandex.Dialogs.Entity where

import Data.Aeson
import Data.Text
import GHC.Generics
import Util.Aeson

data Entity = Entity
    { type' :: Text
    , start :: Int
    , end :: Int
    , value :: Value
    }
    deriving stock (Generic, Show)

instance FromJSON Entity where
    parseJSON = genericParseJSON typeFieldLabelModifier
