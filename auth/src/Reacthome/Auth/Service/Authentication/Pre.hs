module Reacthome.Auth.Service.Authentication.Pre where

import Reacthome.Auth.Domain.Challenge
import Reacthome.Auth.Domain.Credential.PublicKey

data PreAuthentication = PreAuthentication
    { challenge :: Challenge
    , allowCredentials :: [PublicKey]
    }
    deriving stock (Show)
