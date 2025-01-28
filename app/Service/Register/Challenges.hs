module Service.Register.Challenges where

import Service.Challenge

data RegisterChallenges = RegisterChallenges
    { has :: Challenge -> IO Bool
    , get :: IO Challenge
    , remove :: Challenge -> IO ()
    }
