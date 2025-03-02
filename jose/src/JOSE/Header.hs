module JOSE.Header where

import Data.Aeson
import Data.UUID
import GHC.Generics
import JOSE.Alg
import JOSE.Typ

data Header = Header
    { typ :: Typ
    , alg :: Alg
    , kid :: UUID
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

makeHeader :: UUID -> Header
makeHeader = Header JWT EdDSA
