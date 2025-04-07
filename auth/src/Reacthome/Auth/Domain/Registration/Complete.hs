module Reacthome.Auth.Domain.Registration.Complete where

import Data.ByteString
import Reacthome.Auth.Domain.Challenge
import Reacthome.Auth.Domain.Credential.PublicKey.Algorithm
import Reacthome.Auth.Domain.Credential.PublicKey.Id

data CompleteRegistration = CompleteRegistration
    { id :: PublicKeyId
    , challenge :: Challenge
    , publicKey :: ByteString
    , publicKeyAlgorithm :: PublicKeyAlgorithm
    }
    deriving stock (Show)
