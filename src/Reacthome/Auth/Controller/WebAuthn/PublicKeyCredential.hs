module Reacthome.Auth.Controller.WebAuthn.PublicKeyCredential where

import Data.Aeson
import Data.ByteString
import Data.Text
import GHC.Generics
import Reacthome.Auth.Controller.WebAuthn.COSEAlgorithmIdentifier
import Reacthome.Auth.Domain.Credential.PublicKey.Algorithm
import Util.Base64

data PublicKeyCredential t a = PublicKeyCredential
    { id :: t
    , challenge :: t
    , publicKey :: t
    , publicKeyAlgorithm :: a
    }
    deriving stock (Show)

type EncodedPublicKeyCredential = PublicKeyCredential Text COSEAlgorithmIdentifier

deriving stock instance Generic EncodedPublicKeyCredential
deriving anyclass instance FromJSON EncodedPublicKeyCredential

type DecodedPublicKeyCredential = PublicKeyCredential ByteString PublicKeyAlgorithm

decodePublicKeyCredential ::
    EncodedPublicKeyCredential ->
    Either String DecodedPublicKeyCredential
decodePublicKeyCredential credential = do
    uid <- fromBase64 credential.id
    challenge <- fromBase64 credential.challenge
    publicKey <- fromBase64 credential.publicKey
    publicKeyAlgorithm <- decodePublicKeyAlgorithm credential.publicKeyAlgorithm
    pure
        PublicKeyCredential
            { id = uid
            , challenge
            , publicKey
            , publicKeyAlgorithm
            }
