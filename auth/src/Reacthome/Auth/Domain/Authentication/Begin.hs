module Reacthome.Auth.Domain.Authentication.Begin where

import Reacthome.Auth.Domain.User.Login

newtype BeginAuthentication = BeginAuthentication
    { login :: UserLogin
    }
    deriving stock (Show)
