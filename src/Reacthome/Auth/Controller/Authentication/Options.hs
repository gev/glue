module Reacthome.Auth.Controller.Authentication.Options where

import Data.Aeson
import Data.Text
import GHC.Generics

newtype AuthenticationOptions = AuthenticationOptions
    { login :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
