module Reacthome.Auth.Controller.WebAuthn.RegisterOptions where

import Data.Aeson
import Data.Text
import GHC.Generics
import Prelude hiding (null)

data RegisterOptions = RegisterOptions
    { name :: Text
    , displayName :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
