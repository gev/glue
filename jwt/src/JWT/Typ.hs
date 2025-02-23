module JWT.Typ where

import Data.Aeson
import GHC.Generics

data Typ = JWT
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)
