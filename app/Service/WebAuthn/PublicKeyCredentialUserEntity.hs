module Service.WebAuthn.PublicKeyCredentialUserEntity where

import Crypto.Random
import Data.Aeson
import Data.ByteString
import Data.Text
import Environment
import GHC.Generics
import Service.WebAuthn.RegisterOptions
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
