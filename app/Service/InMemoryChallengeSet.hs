module Service.InMemoryChallengeSet where

import Control.Concurrent.MVar
import Data.HashSet
import Service.Challenge
import Util.MVar

mkInMemoryChallengeSet :: IO ChallengeSet
mkInMemoryChallengeSet = do
    set <- newMVar empty
    pure
        ChallengeSet
            { has = runRead set . member
            , put = runModify set . insert
            , remove = runModify set . delete
            }
