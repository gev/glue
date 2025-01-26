module Service.WebAuthn.PublicKeyCredential where

import Data.Aeson
import Data.ByteString
import Data.Text
import GHC.Generics
import GHC.IO
import Service.WebAuthn.AuthenticatorAttestationResponse
import Util.Base64

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

decodedPublicKeyCredential ::
    EncodedPublicKeyCredential ->
    Maybe DecodedPublicKeyCredential
decodedPublicKeyCredential credential = do
    let _ = unsafePerformIO do print credential
    uid <- fromBase64 credential.id
    response <- decodedAuthenticatorAttestationResponse credential.response
    pure
        PublicKeyCredential
            { id = uid
            , authenticatorAttachment = credential.authenticatorAttachment
            , response
            }
