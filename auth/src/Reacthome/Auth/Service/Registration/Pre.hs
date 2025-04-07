module Reacthome.Auth.Service.Registration.Pre where

import Reacthome.Auth.Domain.Challenge
import Reacthome.Auth.Domain.User

data PreRegistration = PreRegistration
    { user :: User
    , challenge :: Challenge
    }
    deriving stock (Show)
