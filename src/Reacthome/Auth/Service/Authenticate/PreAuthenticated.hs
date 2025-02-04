module Reacthome.Auth.Service.Authenticate.PreAuthenticated where

import Reacthome.Auth.Domain.Credential.PublicKey
import Reacthome.Auth.Service.Challenge

data PreAuthenticated = PreAuthenticated
    { challenge :: Challenge
    , allowCredentials :: [PublicKey]
    }
    deriving stock (Show)
