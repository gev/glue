module Reacthome.Assist.Domain.Users where

import Control.Monad.Trans.Maybe
import Reacthome.Assist.Domain.User
import Reacthome.Assist.Domain.User.Id

newtype Users = Users
    { findById :: UserId -> MaybeT IO User
    }
