module Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialUserEntity where

import Data.Aeson
import Data.Text.Lazy qualified as Lazy
import GHC.Generics
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.UserId
import Reacthome.Auth.Domain.UserLogin
import Reacthome.Auth.Domain.UserName

import Data.Text
import Data.UUID
import Util.Base64.Lazy

data PublicKeyCredentialUserEntity = PublicKeyCredentialUserEntity
    { id :: Lazy.Text
    , name :: Text
    , displayName :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)

mkPublicKeyCredentialUserEntity ::
    User ->
    PublicKeyCredentialUserEntity
mkPublicKeyCredentialUserEntity user =
    PublicKeyCredentialUserEntity
        { id = toBase64 . toByteString $ user.id.value
        , name = user.login.value
        , displayName = user.name.value
        }
