module Service.WebAuthn.PublicKeyCredentialDescriptor where

import Data.Aeson
import Data.Text
import GHC.Generics
import Util.Aeson

data PublicKeyCredentialDescriptor = PublicKeyCredentialDescriptor
    { type' :: Text
    , id :: Text
    , transports :: Maybe [Text]
    }
    deriving (Generic, Show)

instance ToJSON PublicKeyCredentialDescriptor where
    toJSON = genericToJSON omitNothing
