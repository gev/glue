module Service.WebAuthn.AuthenticatorAttestationResponse where

import Data.Aeson
import Data.ByteString
import Data.Text
import Data.Text.Lazy qualified as Lazy
import GHC.Generics
import Service.WebAuthn.ClientDataJSON
import Util.Base64
import Util.Base64.Lazy qualified as Lazy

data AuthenticatorAttestationResponse a c = AuthenticatorAttestationResponse
    { attestationObject :: a
    , clientDataJSON :: c
    }
    deriving (Show)

type EncodedAuthenticatorAttestationResponse =
    AuthenticatorAttestationResponse Text Lazy.Text

deriving instance Generic EncodedAuthenticatorAttestationResponse
deriving instance FromJSON EncodedAuthenticatorAttestationResponse

type DecodedAuthenticatorAttestationResponse =
    AuthenticatorAttestationResponse ByteString DecodedClientDataJSON

decodeAuthenticatorAttestationResponse ::
    EncodedAuthenticatorAttestationResponse ->
    Either String DecodedAuthenticatorAttestationResponse
decodeAuthenticatorAttestationResponse response = do
    attestationObject <- fromBase64 response.attestationObject
    clientDataJSON <-
        decodeClientDataJSON
            =<< eitherDecode
            =<< Lazy.fromBase64 response.clientDataJSON
    pure
        AuthenticatorAttestationResponse
            { attestationObject
            , clientDataJSON
            }
