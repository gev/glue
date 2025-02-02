module Reacthome.Auth.Controller.WebAuthn.PublicKeyCredential where

import Data.Aeson
import Data.Text
import GHC.Generics
import Reacthome.Auth.Controller.WebAuthn.COSEAlgorithmIdentifier

data PublicKeyCredential = PublicKeyCredential
    { id :: Text
    , challenge :: Text
    , publicKey :: Text
    , publicKeyAlgorithm :: COSEAlgorithmIdentifier
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
