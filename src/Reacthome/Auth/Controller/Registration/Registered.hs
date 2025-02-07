module Reacthome.Auth.Controller.Registration.Registered where

import Data.Aeson
import GHC.Generics
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialRpEntity
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialUserEntity
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Environment

data RegisteredUser = Registered
    { rp :: PublicKeyCredentialRpEntity
    , user :: PublicKeyCredentialUserEntity
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)

makeRegistered ::
    (?environment :: Environment) =>
    User ->
    RegisteredUser
makeRegistered user =
    Registered
        { rp = makePublicKeyCredentialRpEntity
        , user = makePublicKeyCredentialUserEntity user
        }
