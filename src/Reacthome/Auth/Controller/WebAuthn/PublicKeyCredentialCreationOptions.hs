module Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialCreationOptions where

import Data.Aeson
import Data.Text
import GHC.Generics
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialRpEntity
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialUserEntity
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Challenge
import Reacthome.Auth.Service.Register.PreRegistered
import Util.Aeson
import Util.Base64

data PublicKeyCredentialCreationOptions = PublicKeyCredentialCreationOptions
    { rp :: PublicKeyCredentialRpEntity
    , user :: PublicKeyCredentialUserEntity
    , challenge :: Text
    , timeout :: Int
    }
    deriving stock (Generic, Show)

instance ToJSON PublicKeyCredentialCreationOptions where
    toJSON = genericToJSON omitNothing

mkPublicKeyCredentialCreationOptions ::
    (?environment :: Environment) =>
    PreRegistered ->
    PublicKeyCredentialCreationOptions
mkPublicKeyCredentialCreationOptions pre =
    PublicKeyCredentialCreationOptions
        { rp = mkPublicKeyCredentialRpEntity
        , user = mkPublicKeyCredentialUserEntity pre.user
        , challenge = toBase64 pre.challenge.value
        , timeout = ?environment.timeout
        }
