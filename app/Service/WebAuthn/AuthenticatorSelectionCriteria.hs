module Service.WebAuthn.AuthenticatorSelectionCriteria where

import Data.Aeson
import Data.Text
import GHC.Generics
import Util.Aeson

data AuthenticatorSelectionCriteria = AuthenticatorSelectionCriteria
    { authenticatorAttachment :: Maybe Text
    , residentKey :: Maybe Text
    , requireResidentKey :: Maybe Bool
    , userVerification :: Maybe Text
    }
    deriving (Generic, Show)

instance ToJSON AuthenticatorSelectionCriteria where
    toJSON = genericToJSON omitNothing
