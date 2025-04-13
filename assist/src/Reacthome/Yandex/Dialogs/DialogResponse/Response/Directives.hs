module Reacthome.Yandex.Dialogs.DialogResponse.Response.Directives where

import Data.Aeson
import GHC.Generics

newtype Directives = Directives
    { start_account_linking :: Maybe Value
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)

start'account'linking :: Directives
start'account'linking =
    Directives{start_account_linking = Just $ object []}
