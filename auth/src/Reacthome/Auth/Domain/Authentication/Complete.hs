module Reacthome.Auth.Domain.Authentication.Complete where

import Data.ByteString
import Reacthome.Auth.Domain.Challenge
import Reacthome.Auth.Domain.Credential.PublicKey.Id

data CompleteAuthentication = CompleteAuthentication
    { id :: PublicKeyId
    , challenge :: Challenge
    , message :: ByteString
    , signature :: ByteString
    }
    deriving stock (Show)
