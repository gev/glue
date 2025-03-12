module Reacthome.Auth.Service.RefreshTokens where

import Control.Monad.Trans.Maybe
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Service.Challenge

data RefreshTokens = RefreshTokens
    { register :: User -> IO Challenge
    , findBy :: Challenge -> MaybeT IO User
    , remove :: Challenge -> IO ()
    }
