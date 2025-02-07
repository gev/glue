module Reacthome.Auth.Controller.Registration.Options where

import Data.Aeson
import Data.Text
import GHC.Generics

data RegistrationOptions = RegistrationOptions
    { login :: Text
    , name :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
