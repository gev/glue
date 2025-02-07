module Reacthome.Auth.Domain.Authenticate.Begin where

import Reacthome.Auth.Domain.User.Login

newtype BeginAuthenticate = BeginAuthenticate
    { login :: UserLogin
    }
    deriving stock (Show)
