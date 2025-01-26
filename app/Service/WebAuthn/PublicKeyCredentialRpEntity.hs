module Service.WebAuthn.PublicKeyCredentialRpEntity where

import Data.Aeson
import Data.Text
import GHC.Generics
import Util.Aeson

data PublicKeyCredentialRpEntity = PublicKeyCredentialRpEntity
    { id :: Maybe Text
    , name :: Text
    }
    deriving (Generic, Show)

instance ToJSON PublicKeyCredentialRpEntity where
    toJSON = genericToJSON omitNothing
