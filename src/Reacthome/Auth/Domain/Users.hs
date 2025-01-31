module Reacthome.Auth.Domain.Users where

import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.User.Id
import Reacthome.Auth.Domain.User.Login

data Users = Users
    { findById :: UserId -> IO (Maybe User)
    , findByLogin :: UserLogin -> IO (Maybe User)
    , has :: UserLogin -> IO Bool
    , store :: User -> IO (Either String ())
    , remove :: User -> IO ()
    }
