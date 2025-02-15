module Reacthome.Auth.Service.Challenges where

import Control.Monad.Trans.Maybe
import Reacthome.Auth.Service.Challenge

data Challenges t = Challenges
    { makeNew :: t -> IO Challenge
    , findBy :: Challenge -> MaybeT IO t
    , remove :: Challenge -> IO ()
    }
