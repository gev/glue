module Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialCreationOptions where

import Data.Aeson
import Data.ByteString
import Data.Text
import GHC.Generics
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialRpEntity
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialUserEntity
import Util.Aeson
import Util.Base64

data PublicKeyCredentialCreationOptions = PublicKeyCredentialCreationOptions
    { rp :: PublicKeyCredentialRpEntity
    , user :: PublicKeyCredentialUserEntity
    , challenge :: Text
    , timeout :: Maybe Int
    }
    deriving stock (Generic, Show)

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
        , timeout = Just timeout
        }
