module Reacthome.Auth.Controller.WebAuthn.AuthenticatorAttestationResponse where

import Data.Aeson
import Data.Text
import GHC.Generics
import Reacthome.Auth.Controller.WebAuthn.COSEAlgorithmIdentifier

data AuthenticatorAttestationResponse = AuthenticatorAttestationResponse
    { challenge :: Text
    , publicKey :: Text
    , publicKeyAlgorithm :: COSEAlgorithmIdentifier
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
