module Service.WebAuthn.PublicKeyCredential where

import Data.Aeson
import Data.ByteString
import Data.Text
import GHC.Generics
import Service.WebAuthn.AuthenticatorAttestationResponse
import Util.Base64.URL

data PublicKeyCredential t r = PublicKeyCredential
    { id :: t
    , authenticatorAttachment :: Text
    , response :: r
    }
    deriving (Show)

type EncodedPublicKeyCredential =
    PublicKeyCredential
        Text
        EncodedAuthenticatorAttestationResponse

deriving instance Generic EncodedPublicKeyCredential
deriving instance FromJSON EncodedPublicKeyCredential

type DecodedPublicKeyCredential =
    PublicKeyCredential
        ByteString
        DecodedAuthenticatorAttestationResponse

decodePublicKeyCredential ::
    EncodedPublicKeyCredential ->
    Either String DecodedPublicKeyCredential
decodePublicKeyCredential credential = do
    uid <- fromBase64 credential.id
    response <- decodeAuthenticatorAttestationResponse credential.response
    pure
        PublicKeyCredential
            { id = uid
            , authenticatorAttachment = credential.authenticatorAttachment
            , response
            }
