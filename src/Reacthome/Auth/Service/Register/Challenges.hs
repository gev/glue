module Reacthome.Auth.Service.Register.Challenges where

import Control.Monad.Trans.Maybe
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Service.Challenge

data RegisterChallenges = RegisterChallenges
    { register :: User -> IO Challenge
    , findBy :: Challenge -> MaybeT IO User
    , remove :: Challenge -> IO ()
    }
