module Reacthome.Auth.Controller.Authenticate.AuthenticatedUser where

import Data.Aeson
import GHC.Generics
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialRpEntity
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialUserEntity
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Environment

data AuthenticatedUser = AuthenticatedUser
    { rp :: PublicKeyCredentialRpEntity
    , user :: PublicKeyCredentialUserEntity
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)

mkAuthenticatedUser ::
    (?environment :: Environment) =>
    User ->
    AuthenticatedUser
mkAuthenticatedUser user =
    AuthenticatedUser
        { rp = mkPublicKeyCredentialRpEntity
        , user = mkPublicKeyCredentialUserEntity user
        }
