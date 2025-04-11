module Reacthome.Auth.Domain.Users where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.User.Id
import Reacthome.Auth.Domain.User.Login

data Users = Users
    { getAll :: IO [User]
    , findById :: UserId -> MaybeT IO User
    , findByLogin :: UserLogin -> MaybeT IO User
    , has :: UserLogin -> IO Bool
    , store :: User -> ExceptT String IO ()
    , remove :: User -> IO ()
    }
