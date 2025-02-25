module JOSE.Typ where

import Data.Aeson
import GHC.Generics
import JOSE.Util

data Typ = JWT
    deriving stock (Generic, Show)

instance FromJSON Typ where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Typ where
    toJSON = genericToJSON aesonOptions
