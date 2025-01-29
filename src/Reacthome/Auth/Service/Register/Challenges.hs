module Reacthome.Auth.Service.Register.Challenges where

import Reacthome.Auth.Service.Challenge
import Reacthome.Auth.Service.WebAuthn.RegisterOptions

data RegisterChallenges = RegisterChallenges
    { register :: ValidRegisterOptions -> IO Challenge
    , get :: Challenge -> IO (Either String ValidRegisterOptions)
    , remove :: Challenge -> IO ()
    }
