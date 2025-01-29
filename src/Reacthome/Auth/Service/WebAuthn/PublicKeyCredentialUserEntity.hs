module Reacthome.Auth.Service.WebAuthn.PublicKeyCredentialUserEntity where

import Crypto.Random
import Data.Aeson
import Data.ByteString
import Data.Text
import GHC.Generics
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.WebAuthn.RegisterOptions
import Util.Base64

data PublicKeyCredentialUserEntity = PublicKeyCredentialUserEntity
    { id :: Text
    , name :: Text
    , displayName :: Text
    }
    deriving (Generic, Show, ToJSON)

mkRandomUserId :: (?environment :: Environment) => IO ByteString
mkRandomUserId = getRandomBytes ?environment.userIdSize

mkPublicKeyCredentialUserEntity ::
    ByteString ->
    ValidRegisterOptions ->
    PublicKeyCredentialUserEntity
mkPublicKeyCredentialUserEntity uid register =
    PublicKeyCredentialUserEntity
        { id = toBase64 uid
        , name = register.options.name
        , displayName = register.options.displayName
        }
