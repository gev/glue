module Reacthome.Auth.Domain.Users where

import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.User.Id
import Reacthome.Auth.Domain.User.Login

data Users = Users
    { getById :: UserId -> IO (Either String User)
    , getByLogin :: UserLogin -> IO (Either String User)
    , store :: User -> IO (Either String ())
    , remove :: User -> IO ()
    }
