module Reacthome.Auth.Controller.WebAuthn.AuthenticatorAssertionResponse where

import Data.Aeson
import Data.Text
import GHC.Generics

data AuthenticatorAssertionResponse = AuthenticatorAssertionResponse
    { challenge :: Text
    , signedData :: Text
    , signature :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
