module Reacthome.Auth.Controller.WebAuthn.PublicKeyCredential where

import Data.Aeson
import Data.Text
import GHC.Generics

data PublicKeyCredential a = PublicKeyCredential
    { id :: Text
    , response :: a
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
