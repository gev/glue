module Reacthome.Auth.Domain.Users where

import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.UserId
import Reacthome.Auth.Domain.UserLogin (UserLogin)

data Users = Users
    { getById :: UserId -> IO (Either String User)
    , getByLogin :: UserLogin -> IO (Either String User)
    , store :: User -> IO (Either String ())
    , remove :: User -> IO ()
    }
