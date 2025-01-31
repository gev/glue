module Reacthome.Auth.Domain.Users where

import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.User.Id
import Reacthome.Auth.Domain.User.Login

data Users = Users
    { findById :: UserId -> IO (Either String User)
    , findByLogin :: UserLogin -> IO (Either String User)
    , has :: UserLogin -> IO Bool
    , store :: User -> IO (Either String ())
    , remove :: User -> IO ()
    }
