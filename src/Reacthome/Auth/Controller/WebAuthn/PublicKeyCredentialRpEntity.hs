module Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialRpEntity where

import Data.Aeson
import Data.Text
import GHC.Generics
import Reacthome.Auth.Environment
import Util.Aeson

data PublicKeyCredentialRpEntity = PublicKeyCredentialRpEntity
    { id :: Maybe Text
    , name :: Text
    }
    deriving stock (Generic, Show)

instance ToJSON PublicKeyCredentialRpEntity where
    toJSON = genericToJSON omitNothing

makePublicKeyCredentialRpEntity ::
    (?environment :: Environment) =>
    PublicKeyCredentialRpEntity
makePublicKeyCredentialRpEntity =
    PublicKeyCredentialRpEntity
        { id = Just ?environment.domain
        , name = ?environment.name
        }
