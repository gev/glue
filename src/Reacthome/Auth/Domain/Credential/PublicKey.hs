module Reacthome.Auth.Domain.Credential.PublicKey where

import Data.ByteString
import Reacthome.Auth.Domain.Credential.PublicKey.Algorithm
import Reacthome.Auth.Domain.Credential.PublicKey.Id

data PublicKey = PublicKey
    { id :: PublicKeyId
    , algorithm :: PublicKeyAlgorithm
    , publicKey :: ByteString
    }
    deriving stock (Show)
