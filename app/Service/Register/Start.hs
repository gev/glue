module Service.Register.Start where

import Crypto.Random
import Data.Aeson
import Data.ByteString
import Data.Text
import Environment
import GHC.Generics
import Util.Base64

{--
    RFC: https://w3c.github.io/webauthn/#dictdef-publickeycredentialcreationoption
--}

data StartRegisterOptions = StartRegisterOptions
    { name :: Text
    , displayName :: Text
    }
    deriving (Generic, Show)
instance FromJSON StartRegisterOptions

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

data PublicKeyCredentialUserEntity = PublicKeyCredentialUserEntity
    { id :: Text
    , name :: Text
    , displayName :: Text
    }
    deriving (Generic, Show)
instance ToJSON PublicKeyCredentialUserEntity

data PublicKeyCredentialRpEntity = PublicKeyCredentialRpEntity
    { id :: Maybe Text
    , name :: Text
    }
    deriving (Generic, Show)
instance ToJSON PublicKeyCredentialRpEntity where
    toJSON = genericToJSON omitNothing

data PublicKeyCredentialParameters = PublicKeyCredentialParameters
    { type' :: Text
    , alg :: COSEAlgorithmIdentifier
    }
    deriving (Generic, Show)
instance ToJSON PublicKeyCredentialParameters where
    toJSON = genericToJSON typeFieldLabelModifier

data PublicKeyCredentialDescriptor = PublicKeyCredentialDescriptor
    { type' :: Text
    , id :: Text
    , transports :: Maybe [Text]
    }
    deriving (Generic, Show)
instance ToJSON PublicKeyCredentialDescriptor where
    toJSON = genericToJSON omitNothing

data AuthenticatorSelectionCriteria = AuthenticatorSelectionCriteria
    { authenticatorAttachment :: Maybe Text
    , residentKey :: Maybe Text
    , requireResidentKey :: Maybe Bool
    , userVerification :: Maybe Text
    }
    deriving (Generic, Show)
instance ToJSON AuthenticatorSelectionCriteria where
    toJSON = genericToJSON omitNothing

type COSEAlgorithmIdentifier = Int

ed25519 :: COSEAlgorithmIdentifier
ed25519 = -8

es256 :: COSEAlgorithmIdentifier
es256 = -7

rs256 :: COSEAlgorithmIdentifier
rs256 = -257

publicKeyCredentialType :: Text
publicKeyCredentialType = "public-key"

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

mkPublicKeyCredentialUserEntity ::
    ByteString ->
    Text ->
    Text ->
    PublicKeyCredentialUserEntity
mkPublicKeyCredentialUserEntity uid name displayName =
    PublicKeyCredentialUserEntity
        { id = toBase64 uid
        , name
        , displayName
        }

startRegister ::
    (?environment :: Environment) =>
    StartRegisterOptions ->
    IO (Maybe PublicKeyCredentialCreationOptions)
startRegister req = do
    uid <- getRandomBytes 20
    challenge <- getRandomBytes 20
    let name = req.name
    let displayName = req.displayName
    if name /= "" && displayName /= ""
        then do
            let user = mkPublicKeyCredentialUserEntity uid name displayName
            let rp =
                    PublicKeyCredentialRpEntity
                        { id = Just ?environment.domain
                        , name = ?environment.name
                        }
            let timeout = ?environment.timeout
            pure $
                Just $
                    mkPublicKeyCredentialCreationOptions rp user challenge timeout
        else pure Nothing

omitNothing :: Options
omitNothing =
    defaultOptions
        { omitNothingFields = True
        }

typeFieldLabelModifier :: Options
typeFieldLabelModifier =
    defaultOptions
        { fieldLabelModifier = \s ->
            if s == "type'"
                then "type"
                else s
        }
