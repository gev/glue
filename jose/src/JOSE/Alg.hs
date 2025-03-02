module JOSE.Alg where

import Data.Aeson
import GHC.Generics
import JOSE.Util

data Alg = EdDSA
    deriving stock (Generic, Eq, Show)

instance FromJSON Alg where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Alg where
    toJSON = genericToJSON aesonOptions
