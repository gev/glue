module Reacthome.Auth.Domain.Register.Finish where

import Data.ByteString
import Reacthome.Auth.Domain.Credential.PublicKey.Algorithm
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Service.Challenge

data FinishRegister = FinishRegister
    { id :: PublicKeyId
    , challenge :: Challenge
    , publicKey :: ByteString
    , publicKeyAlgorithm :: PublicKeyAlgorithm
    }
    deriving stock (Show)
