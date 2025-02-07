module Reacthome.Auth.Domain.Authenticate.Finish where

import Data.ByteString
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Service.Challenge

data FinishAuthenticate = FinishAuthenticate
    { id :: PublicKeyId
    , challenge :: Challenge
    , message :: ByteString
    , signature :: ByteString
    }
    deriving stock (Show)
