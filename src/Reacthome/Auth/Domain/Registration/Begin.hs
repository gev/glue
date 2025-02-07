module Reacthome.Auth.Domain.Registration.Begin where

import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.User.Name

data BeginRegistration = BeginRegistration
    { login :: UserLogin
    , name :: UserName
    }
    deriving stock (Show)
