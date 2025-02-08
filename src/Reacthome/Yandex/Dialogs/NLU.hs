module Reacthome.Yandex.Dialogs.NLU where

import Data.Aeson
import GHC.Generics
import Reacthome.Yandex.Dialogs.Entity
import Reacthome.Yandex.Dialogs.Intents
import Reacthome.Yandex.Dialogs.Token

data NLU = NLU
    { tokens :: [Token]
    , entities :: [Entity]
    , intents :: Intents
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
