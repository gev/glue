module Reacthome.Auth.Service.Challenges where

import Control.Monad.Trans.Maybe
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Service.Challenge

data Challenges = Challenges
    { registration :: User -> IO Challenge
    , findBy :: Challenge -> MaybeT IO User
    , remove :: Challenge -> IO ()
    }
