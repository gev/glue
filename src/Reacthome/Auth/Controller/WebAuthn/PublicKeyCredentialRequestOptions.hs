module Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialRequestOptions where

import Data.Aeson
import Data.Text
import GHC.Generics
import Reacthome.Auth.Domain.Credential.PublicKey
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Authenticate.PreAuthenticated
import Reacthome.Auth.Service.Challenge
import Util.Aeson
import Util.Base64

data PublicKeyCredentialRequestOptions = PublicKeyCredentialRequestOptions
    { rpId :: Text
    , challenge :: Text
    , timeout :: Int
    , allowCredentials :: [Text]
    }
    deriving stock (Generic, Show)

instance ToJSON PublicKeyCredentialRequestOptions where
    toJSON = genericToJSON omitNothing

mkPublicKeyCredentialRequestOptions ::
    (?environment :: Environment) =>
    PreAuthenticated ->
    PublicKeyCredentialRequestOptions
mkPublicKeyCredentialRequestOptions pre =
    PublicKeyCredentialRequestOptions
        { rpId = ?environment.domain
        , challenge = toBase64 pre.challenge.value
        , timeout = ?environment.timeout
        , allowCredentials = toBase64 . (.value) . (.id) <$> pre.allowCredentials
        }
