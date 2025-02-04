module Reacthome.Auth.Controller.Authenticate.AuthenticateOptions where

import Data.Aeson
import Data.Text
import GHC.Generics

newtype AuthenticateOptions = AuthenticateOptions
    { login :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
