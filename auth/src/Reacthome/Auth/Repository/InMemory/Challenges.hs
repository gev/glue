module Reacthome.Auth.Repository.InMemory.Challenges where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict
import Reacthome.Auth.Service.Challenge
import Reacthome.Auth.Service.Challenges
import Util.MVar
import Prelude hiding (lookup)

makeChallenges :: IO (Challenges t)
makeChallenges = do
    map' <- newMVar empty
    let
        makeNew ttl payload = do
            challenge <- makeRandomChallenge ttl
            runModify map' $ insert challenge payload
            void $ forkIO do
                threadDelay $ 1_000_000 * ttl
                remove challenge
            pure challenge

        findBy = MaybeT . runRead map' . lookup

        remove = runModify map' . delete

    pure
        Challenges
            { makeNew
            , findBy
            , remove
            }
