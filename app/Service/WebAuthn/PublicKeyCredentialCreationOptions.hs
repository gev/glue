module Service.WebAuthn.PublicKeyCredentialCreationOptions where

import Data.Aeson
import Data.ByteString
import Data.Text
import GHC.Generics
import Service.WebAuthn.AuthenticatorSelectionCriteria
import Service.WebAuthn.PublicKeyCredentialDescriptor
import Service.WebAuthn.PublicKeyCredentialParameters
import Service.WebAuthn.PublicKeyCredentialRpEntity
import Service.WebAuthn.PublicKeyCredentialUserEntity
import Util.Aeson
import Util.Base64

data PublicKeyCredentialCreationOptions = PublicKeyCredentialCreationOptions
    { rp :: PublicKeyCredentialRpEntity
    , user :: PublicKeyCredentialUserEntity
    , challenge :: Text
    , pubKeyCredParams :: [PublicKeyCredentialParameters]
    , timeout :: Maybe Int
    , excludeCredentials :: Maybe [PublicKeyCredentialDescriptor]
    , authenticatorSelection :: Maybe AuthenticatorSelectionCriteria
    , hints :: Maybe [Text]
    , attestation :: Maybe Text
    , attestationFormats :: Maybe [Text]
    }
    deriving (Generic, Show)

instance ToJSON PublicKeyCredentialCreationOptions where
    toJSON = genericToJSON omitNothing

mkPublicKeyCredentialCreationOptions ::
    PublicKeyCredentialRpEntity ->
    PublicKeyCredentialUserEntity ->
    ByteString ->
    Int ->
    PublicKeyCredentialCreationOptions
mkPublicKeyCredentialCreationOptions rp user challenge timeout =
    PublicKeyCredentialCreationOptions
        { rp
        , user
        , challenge = toBase64 challenge
        , pubKeyCredParams = publicKeyCredentialParameters
        , timeout = Just timeout
        , excludeCredentials = Nothing
        , authenticatorSelection = Nothing
        , hints = Nothing
        , attestation = Nothing
        , attestationFormats = Nothing
        }
