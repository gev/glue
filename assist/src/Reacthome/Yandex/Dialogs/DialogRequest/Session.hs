module Reacthome.Yandex.Dialogs.DialogRequest.Session where

import Data.Aeson
import Data.Text
import GHC.Generics
import Reacthome.Yandex.Dialogs.DialogRequest.Session.Application
import Reacthome.Yandex.Dialogs.DialogRequest.Session.User

data Session = Session
    { message_id :: Int
    , session_id :: Text
    , skill_id :: Text
    , user :: User
    , application :: Application
    , new :: Bool
    , user_id :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
