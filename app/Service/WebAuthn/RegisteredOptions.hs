module Service.WebAuthn.RegisteredOptions where

import Data.Aeson
import Data.Text
import GHC.Generics

data RegisteredOptions = RegisteredOptions
    { res :: Text
    }
    deriving (Generic, Show, ToJSON)
