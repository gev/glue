module Reacthome.Auth.Domain.Users where

import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.User.Id
import Reacthome.Auth.Domain.User.Login

data Users = Users
    { getAll :: IO (Either String [User])
    , findById :: UserId -> IO (Either String User)
    , findByLogin :: UserLogin -> IO (Either String User)
    , has :: UserLogin -> IO (Either String Bool)
    , store :: User -> IO (Either String ())
    , remove :: User -> IO (Either String ())
    }
