module Reacthome.Auth.Service.Register.PreRegistered where

import Reacthome.Auth.Domain.User
import Reacthome.Auth.Service.Challenge

data PreRegistered = PreRegistered
    { user :: User
    , challenge :: Challenge
    }
    deriving stock (Show)
