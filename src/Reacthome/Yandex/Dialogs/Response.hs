module Reacthome.Yandex.Dialogs.Response where

import Data.Aeson
import Data.Text
import GHC.Generics

data Response = Response
    { text :: Text
    , tts :: Maybe Text
    , end_session :: Bool
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)
