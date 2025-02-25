module JOSE.Alg where

import Data.Aeson
import GHC.Generics

data Alg = EdDSA
    deriving stock (Generic, Eq, Show)

instance FromJSON Alg where
    parseJSON = genericParseJSON options

instance ToJSON Alg where
    toJSON = genericToJSON options

options :: Options
options =
    defaultOptions
        { tagSingleConstructors = True
        }
