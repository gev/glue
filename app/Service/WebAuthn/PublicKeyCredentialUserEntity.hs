module Service.WebAuthn.PublicKeyCredentialUserEntity where

import Data.Aeson
import Data.ByteString
import Data.Text
import GHC.Generics
import Util.Base64

data PublicKeyCredentialUserEntity = PublicKeyCredentialUserEntity
    { id :: Text
    , name :: Text
    , displayName :: Text
    }
    deriving (Generic, Show, ToJSON)

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
