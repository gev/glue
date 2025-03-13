module Reacthome.Yandex.Dialogs.DialogRequest where

import Data.Aeson
import Data.Aeson.KeyMap
import Data.Text
import Reacthome.Yandex.Dialogs.Meta
import Reacthome.Yandex.Dialogs.Request
import Reacthome.Yandex.Dialogs.Session

data DialogRequest
    = DialogRequest
        { meta :: Meta
        , session :: Session
        , version :: Text
        , request :: Request
        }
    | LinkingComplete
        { meta :: Meta
        , session :: Session
        , version :: Text
        }
    deriving stock (Show)

instance FromJSON DialogRequest where
    parseJSON = withObject "DialogRequest" \v -> do
        let meta = v .: "meta"
            session = v .: "session"
            version = v .: "version"
        if member "account_linking_complete_event" v
            then
                LinkingComplete
                    <$> meta
                    <*> session
                    <*> version
            else
                DialogRequest
                    <$> meta
                    <*> session
                    <*> version
                    <*> v .: "request"
