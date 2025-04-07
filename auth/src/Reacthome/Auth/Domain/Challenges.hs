module Reacthome.Auth.Domain.Challenges where

import Control.Monad.Trans.Maybe
import Reacthome.Auth.Domain.Challenge

data Challenges t = Challenges
    { makeNew :: Int -> t -> IO Challenge
    , findBy :: Challenge -> MaybeT IO t
    , remove :: Challenge -> IO ()
    }
