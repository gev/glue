module Reacthome.Auth.Controller.Register.RegisteredUser where

import Data.Aeson
import GHC.Generics
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialRpEntity
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialUserEntity
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Environment

data RegisteredUser = RegisteredUser
    { rp :: PublicKeyCredentialRpEntity
    , user :: PublicKeyCredentialUserEntity
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)

makeRegisteredUser ::
    (?environment :: Environment) =>
    User ->
    RegisteredUser
makeRegisteredUser user =
    RegisteredUser
        { rp = makePublicKeyCredentialRpEntity
        , user = makePublicKeyCredentialUserEntity user
        }
