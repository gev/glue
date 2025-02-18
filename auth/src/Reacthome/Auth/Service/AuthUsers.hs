module Reacthome.Auth.Service.AuthUsers where

import Control.Monad.Trans.Maybe
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Service.Challenge

data AuthUsers = AuthUsers
    { register :: User -> IO Challenge
    , findBy :: Challenge -> MaybeT IO User
    , remove :: Challenge -> IO ()
    }
