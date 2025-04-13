module Reacthome.Yandex.Dialogs.DialogResponse.Response where

import Data.Aeson
import Data.Text
import GHC.Generics
import Reacthome.Yandex.Dialogs.DialogResponse.Response.Directives

data Response = Response
    { text :: Text
    , tts :: Maybe Text
    , end_session :: Bool
    , directives :: Maybe Directives
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)
