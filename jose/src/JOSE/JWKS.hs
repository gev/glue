module JOSE.JWKS where

import Data.Aeson
import GHC.Generics
import JOSE.JWK

newtype JWKS = JWKS
    { keys :: [JWK]
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)
