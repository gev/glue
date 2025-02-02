module Reacthome.Auth.Controller.Register.RegisterOptions where

import Data.Aeson
import Data.Text
import GHC.Generics
import Prelude hiding (null)

data RegisterOptions = RegisterOptions
    { login :: Text
    , name :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
