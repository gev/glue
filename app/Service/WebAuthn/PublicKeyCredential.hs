module Service.WebAuthn.PublicKeyCredential where

import Data.Aeson
import Data.ByteString
import Data.Text
import GHC.Generics
import Util.Base64

data PublicKeyCredential t = PublicKeyCredential
    { id :: t
    , challenge :: t
    , publicKey :: t
    , publicKeyAlgorithm :: Int
    }
    deriving (Show)

type EncodedPublicKeyCredential = PublicKeyCredential Text

deriving instance Generic EncodedPublicKeyCredential
deriving instance FromJSON EncodedPublicKeyCredential

type DecodedPublicKeyCredential = PublicKeyCredential ByteString

decodePublicKeyCredential ::
    EncodedPublicKeyCredential ->
    Either String DecodedPublicKeyCredential
decodePublicKeyCredential credential = do
    uid <- fromBase64 credential.id
    challenge <- fromBase64 credential.challenge
    publicKey <- fromBase64 credential.publicKey
    pure
        PublicKeyCredential
            { id = uid
            , challenge
            , publicKey
            , publicKeyAlgorithm = credential.publicKeyAlgorithm
            }
