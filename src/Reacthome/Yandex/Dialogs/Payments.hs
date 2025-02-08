module Reacthome.Yandex.Dialogs.Payments where

import Data.Aeson
import GHC.Generics

data Payments = Payments
    {
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
