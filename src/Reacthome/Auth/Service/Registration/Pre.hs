module Reacthome.Auth.Service.Registration.Pre where

import Reacthome.Auth.Domain.User
import Reacthome.Auth.Service.Challenge

data PreRegistration = PreRegistration
    { user :: User
    , challenge :: Challenge
    }
    deriving stock (Show)
