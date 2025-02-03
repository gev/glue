module Reacthome.Auth.Service.Authenticate.PreAuthenticated where

import Reacthome.Auth.Service.Challenge

newtype PreAuthenticated = PreAuthenticated
    { challenge :: Challenge
    }
    deriving stock (Show)
