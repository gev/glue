module JWT.Header where

import Data.Aeson
import Data.Text
import GHC.Generics
import JWT.Alg
import JWT.Typ

data Header = Header
    { typ :: Typ
    , alg :: Alg
    , kid :: Text
    , jku :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)
