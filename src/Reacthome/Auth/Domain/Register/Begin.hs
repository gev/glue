module Reacthome.Auth.Domain.Register.Begin where

import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.User.Name

data BeginRegister = BeginRegister
    { login :: UserLogin
    , name :: UserName
    }
    deriving stock (Show)
