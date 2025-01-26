module Service.WebAuthn.PublicKeyCredentialParameters where

import Data.Aeson
import GHC.Generics
import Service.WebAuthn.COSEAlgorithmIdentifier
import Service.WebAuthn.PublicKeyCredentialType
import Util.Aeson

data PublicKeyCredentialParameters = PublicKeyCredentialParameters
    { type' :: PublicKeyCredentialType
    , alg :: COSEAlgorithmIdentifier
    }
    deriving (Generic, Show)

instance ToJSON PublicKeyCredentialParameters where
    toJSON = genericToJSON typeFieldLabelModifier

publicKeyCredential :: COSEAlgorithmIdentifier -> PublicKeyCredentialParameters
publicKeyCredential = PublicKeyCredentialParameters publicKeyCredentialType

publicKeyCredentialEd25519 :: PublicKeyCredentialParameters
publicKeyCredentialEd25519 = publicKeyCredential ed25519

publicKeyCredentialES256 :: PublicKeyCredentialParameters
publicKeyCredentialES256 = publicKeyCredential es256

publicKeyCredentialRS256 :: PublicKeyCredentialParameters
publicKeyCredentialRS256 = publicKeyCredential rs256

publicKeyCredentialParameters :: [PublicKeyCredentialParameters]
publicKeyCredentialParameters =
    [ publicKeyCredentialEd25519
    , publicKeyCredentialES256
    , publicKeyCredentialRS256
    ]
