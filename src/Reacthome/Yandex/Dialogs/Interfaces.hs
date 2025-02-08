module Reacthome.Yandex.Dialogs.Interfaces where

import Data.Aeson
import GHC.Generics
import Reacthome.Yandex.Dialogs.AccountLinking
import Reacthome.Yandex.Dialogs.Payments
import Reacthome.Yandex.Dialogs.Screen

data Interfaces = Interfaces
    { screen :: Screen
    , payments :: Payments
    , account_linking :: AccountLinking
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
