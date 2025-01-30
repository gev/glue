module Reacthome.Auth.Domain.Users where

import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.UserId
import Reacthome.Auth.Domain.UserLogin (UserLogin)

data Users = Users
    { getById :: UserId -> IO (Maybe User)
    , getByLogin :: UserLogin -> IO (Maybe User)
    , store :: User -> IO ()
    , remove :: User -> IO ()
    }
