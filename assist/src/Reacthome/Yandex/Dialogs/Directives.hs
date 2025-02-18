module Reacthome.Yandex.Dialogs.Directives where

import Data.Aeson
import GHC.Generics
import Util.Aeson

newtype Directives = Directives
    { start_account_linking :: Maybe Value
    }
    deriving stock (Generic, Show)

instance ToJSON Directives where
    toJSON = genericToJSON typeFieldLabelModifier

start'account'linking :: Directives
start'account'linking =
    Directives{start_account_linking = Just $ object []}
