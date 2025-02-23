module JWT.Alg where

import Data.Aeson
import GHC.Generics

data Alg = EdDSA
    deriving stock (Generic, Eq, Show)
    deriving anyclass (FromJSON, ToJSON)
