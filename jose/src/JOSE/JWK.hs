module JOSE.JWK where

import Data.Aeson
import Data.Text
import Data.UUID
import GHC.Generics
import JOSE.Util

data JWK = JWK
    { kty :: Kty
    , crv :: Crv
    , x :: Text
    , kid :: UUID
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

data Kty = OKP
    deriving stock (Generic, Show)

instance FromJSON Kty where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Kty where
    toJSON = genericToJSON aesonOptions

data Crv = Ed25519
    deriving stock (Generic, Show)

instance FromJSON Crv where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Crv where
    toJSON = genericToJSON aesonOptions
