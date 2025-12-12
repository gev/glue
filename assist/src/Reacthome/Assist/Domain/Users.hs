module Reacthome.Assist.Domain.Users where

import Reacthome.Assist.Domain.User
import Reacthome.Assist.Domain.User.Id

newtype Users = Users
    { findById :: UserId -> Either String User
    }
