module Reacthome.Auth.Service.Register.Challenges where

import Reacthome.Auth.Domain.User
import Reacthome.Auth.Service.Challenge

data RegisterChallenges = RegisterChallenges
    { register :: User -> IO Challenge
    , findBy :: Challenge -> IO (Maybe User)
    , remove :: Challenge -> IO ()
    }
