module Reacthome.Auth.Domain.Register.Start where

import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.User.Name

data StartRegister = StartRegister
    { login :: UserLogin
    , name :: UserName
    }
    deriving stock (Show)
