module Reacthome.Yandex.Dialogs.DialogRequest.Request where

import Data.Aeson
import Data.Text
import GHC.Generics
import Reacthome.Yandex.Dialogs.DialogRequest.Request.Markup
import Util.Aeson

data Request = Request
    { command :: Text
    , original_utterance :: Text
    , markup :: Markup
    , type' :: Text
    }
    deriving stock (Generic, Show)

instance FromJSON Request where
    parseJSON = genericParseJSON typeFieldLabelModifier
