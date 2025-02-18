module Reacthome.Auth.Domain.Registration.Complete where

import Data.ByteString
import Reacthome.Auth.Domain.Credential.PublicKey.Algorithm
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Service.Challenge

data CompleteRegistration = CompleteRegistration
    { id :: PublicKeyId
    , challenge :: Challenge
    , publicKey :: ByteString
    , publicKeyAlgorithm :: PublicKeyAlgorithm
    }
    deriving stock (Show)
