module Reacthome.Auth.Controller.Authentication.Authenticated where

import Data.Aeson
import GHC.Generics
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialRpEntity
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialUserEntity
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Environment

data Authenticated = Authenticated
    { rp :: PublicKeyCredentialRpEntity
    , user :: PublicKeyCredentialUserEntity
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)

makeAuthenticated ::
    (?environment :: Environment) =>
    User ->
    Authenticated
makeAuthenticated user =
    Authenticated
        { rp = makePublicKeyCredentialRpEntity
        , user = makePublicKeyCredentialUserEntity user
        }
