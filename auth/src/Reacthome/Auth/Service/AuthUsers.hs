module Reacthome.Auth.Service.AuthUsers where

import Control.Monad.Trans.Maybe
import Reacthome.Auth.Domain.Challenge
import Reacthome.Auth.Domain.User

data AuthUsers = AuthUsers
    { register :: User -> IO Challenge
    , findBy :: Challenge -> MaybeT IO User
    , remove :: Challenge -> IO ()
    }
