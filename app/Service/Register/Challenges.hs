module Service.Register.Challenges where

import Service.Challenge
import Service.WebAuthn.RegisterOptions (ValidRegisterOptions)

data RegisterChallenges = RegisterChallenges
    { register :: ValidRegisterOptions -> IO Challenge
    , get :: Challenge -> IO (Maybe ValidRegisterOptions)
    , remove :: Challenge -> IO ()
    }
