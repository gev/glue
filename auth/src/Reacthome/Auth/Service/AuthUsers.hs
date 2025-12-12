module Reacthome.Auth.Service.AuthUsers where

import Reacthome.Auth.Domain.Challenge
import Reacthome.Auth.Domain.User

data AuthUsers = AuthUsers
    { register :: User -> IO Challenge
    , findBy :: Challenge -> IO (Either String User)
    , remove :: Challenge -> IO ()
    }
