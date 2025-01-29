module Reacthome.Auth.Service.WebAuthn.PublicKeyCredentialRpEntity where

import Data.Aeson
import Data.Text
import GHC.Generics
import Reacthome.Auth.Environment
import Util.Aeson

data PublicKeyCredentialRpEntity = PublicKeyCredentialRpEntity
    { id :: Maybe Text
    , name :: Text
    }
    deriving (Generic, Show)

instance ToJSON PublicKeyCredentialRpEntity where
    toJSON = genericToJSON omitNothing

mkPublicKeyCredentialRpEntity ::
    (?environment :: Environment) =>
    PublicKeyCredentialRpEntity
mkPublicKeyCredentialRpEntity =
    PublicKeyCredentialRpEntity
        { id = Just ?environment.domain
        , name = ?environment.name
        }
