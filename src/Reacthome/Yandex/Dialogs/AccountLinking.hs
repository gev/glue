module Reacthome.Yandex.Dialogs.AccountLinking where

import Data.Aeson
import GHC.Generics

data AccountLinking = AccountLinking
    {
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
