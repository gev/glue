module Reacthome.Auth.Controller.WebAuthn.RegisteredOptions where

import Data.Aeson
import GHC.Generics
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialRpEntity
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialUserEntity

data RegisteredOptions = RegisteredOptions
    { rp :: PublicKeyCredentialRpEntity
    , user :: PublicKeyCredentialUserEntity
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)
