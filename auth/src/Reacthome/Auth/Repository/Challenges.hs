module Reacthome.Auth.Repository.Challenges where

import Control.Concurrent
import Control.Error (note)
import Control.Monad
import Data.HashMap.Strict
import Reacthome.Auth.Domain.Challenge
import Reacthome.Auth.Domain.Challenges
import Reacthome.Auth.Environment
import Util.MVar
import Prelude hiding (lookup)

makeChallenges :: (?environment :: Environment) => IO (Challenges t)
makeChallenges = do
    map' <- newMVar empty
    let
        makeNew ttl payload = do
            challenge <- makeRandomChallenge
            runModify map' $ insert challenge payload
            void $ forkIO do
                threadDelay $ 1_000_000 * ttl
                remove challenge
            pure challenge

        findBy hash =
            note ("Challenge " <> show hash <> " not found")
                <$> runRead map' (lookup hash)

        remove = runModify map' . delete

    pure Challenges{..}
