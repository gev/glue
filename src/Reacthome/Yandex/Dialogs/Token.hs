module Reacthome.Yandex.Dialogs.Token where

import Data.Aeson
import GHC.Generics

data Token = Token
    {}
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
