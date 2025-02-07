module Reacthome.Auth.Domain.Authenticate.Complete where

import Data.ByteString
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Service.Challenge

data CompleteAuthenticate = CompleteAuthenticate
    { id :: PublicKeyId
    , challenge :: Challenge
    , message :: ByteString
    , signature :: ByteString
    }
    deriving stock (Show)
