module Reacthome.Auth.Domain.Authenticate.Start where

import Reacthome.Auth.Domain.User.Login

newtype StartAuthenticate = StartAuthenticate
    { login :: UserLogin
    }
    deriving stock (Show)
