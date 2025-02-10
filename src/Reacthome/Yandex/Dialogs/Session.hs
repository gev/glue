module Reacthome.Yandex.Dialogs.Session where

import Data.Aeson
import Data.Text
import GHC.Generics
import Reacthome.Yandex.Dialogs.Application
import Reacthome.Yandex.Dialogs.User

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
