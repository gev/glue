module Reacthome.Auth.Domain.Credential.PublicKey where

import Data.ByteString
import Reacthome.Auth.Domain.Credential.PublicKey.Algorithm
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Domain.User.Id

data PublicKey = PublicKey
    { id :: PublicKeyId
    , userId :: UserId
    , algorithm :: PublicKeyAlgorithm
    , publicKey :: ByteString
    }
    deriving stock (Show)
