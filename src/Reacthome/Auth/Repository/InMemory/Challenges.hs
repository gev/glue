module Reacthome.Auth.Repository.InMemory.Challenges where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Challenge
import Reacthome.Auth.Service.Challenges
import Util.MVar
import Prelude hiding (lookup)

mkChallenges :: (?environment :: Environment) => IO Challenges
mkChallenges = do
    map' <- newMVar empty
    let
        register options = do
            challenge <- mkRandomChallenge ?environment.challengeSize
            runModify map' $ insert challenge options
            void $ forkIO do
                threadDelay $ 1_000 * ?environment.timeout
                remove challenge
            pure challenge

        findBy = MaybeT . runRead map' . lookup

        remove = runModify map' . delete

    pure
        Challenges
            { register
            , findBy
            , remove
            }
