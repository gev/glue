module Reacthome.Auth.Domain.Challenges where

import Reacthome.Auth.Domain.Challenge

data Challenges t = Challenges
    { makeNew :: Int -> t -> IO Challenge
    , findBy :: Challenge -> IO (Either String t)
    , remove :: Challenge -> IO ()
    }
