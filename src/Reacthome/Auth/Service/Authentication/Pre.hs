module Reacthome.Auth.Service.Authentication.Pre where

import Reacthome.Auth.Domain.Credential.PublicKey
import Reacthome.Auth.Service.Challenge

data PreAuthentication = PreAuthentication
    { challenge :: Challenge
    , allowCredentials :: [PublicKey]
    }
    deriving stock (Show)
