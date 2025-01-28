module Service.Register.Challenges where

import Service.Challenge
import Service.WebAuthn.RegisterOptions

data RegisterChallenges = RegisterChallenges
    { register :: ValidRegisterOptions -> IO Challenge
    , get :: Challenge -> IO (Either String ValidRegisterOptions)
    , remove :: Challenge -> IO ()
    }
