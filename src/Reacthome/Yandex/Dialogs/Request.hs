module Reacthome.Yandex.Dialogs.Request where

import Data.Aeson
import Data.Text
import GHC.Generics
import Reacthome.Yandex.Dialogs.Markup
import Reacthome.Yandex.Dialogs.NLU
import Util.Aeson

data Request = Request
    { command :: Text
    , original_utterance :: Text
    , nlu :: NLU
    , markup :: Markup
    , type' :: Text
    }
    deriving stock (Generic, Show)

instance FromJSON Request where
    parseJSON = genericParseJSON typeFieldLabelModifier
