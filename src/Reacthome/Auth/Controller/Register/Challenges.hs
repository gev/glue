module Reacthome.Auth.Controller.Register.Challenges where

import Reacthome.Auth.Controller.Challenge
import Reacthome.Auth.Domain.User

data RegisterChallenges = RegisterChallenges
    { register :: User -> IO Challenge
    , get :: Challenge -> IO (Either String User)
    , remove :: Challenge -> IO ()
    }
