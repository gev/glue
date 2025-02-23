module JWT.Payload where

import Data.Aeson
import Data.Text
import GHC.Generics

data Payload = Payload
    { jti :: UUID
    , iss :: Text
    , sub :: Text
    , exp :: Int
    , iat :: Int
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)
