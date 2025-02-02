module Reacthome.Auth.Controller.WebAuthn.RegisteredOptions where

import Data.Aeson
import GHC.Generics
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialRpEntity
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialUserEntity
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Environment

data RegisteredOptions = RegisteredOptions
    { rp :: PublicKeyCredentialRpEntity
    , user :: PublicKeyCredentialUserEntity
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)

mkRegisteredOptions ::
    (?environment :: Environment) =>
    User ->
    RegisteredOptions
mkRegisteredOptions user =
    RegisteredOptions
        { rp = mkPublicKeyCredentialRpEntity
        , user = mkPublicKeyCredentialUserEntity user
        }
