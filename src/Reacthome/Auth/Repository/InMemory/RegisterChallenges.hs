module Reacthome.Auth.Repository.InMemory.RegisterChallenges where

import Control.Concurrent
import Control.Monad
import Data.HashMap.Strict
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Challenge
import Reacthome.Auth.Service.Register.Challenges
import Util.MVar
import Prelude hiding (lookup)

mkRegisterChallenges :: (?environment :: Environment) => IO RegisterChallenges
mkRegisterChallenges = do
    map' <- newMVar empty
    let get k = do
            options <- runRead map' $ lookup k
            pure case options of
                Nothing -> Left "Challenge not found"
                Just options' -> Right options'

        remove k = runModify map' $ delete k

        register options = do
            challenge <- mkRandomChallenge ?environment.challengeSize
            runModify map' $ insert challenge options
            void $ forkIO do
                threadDelay $ 1_000 * ?environment.timeout
                remove challenge
            pure challenge

    pure
        RegisterChallenges
            { register
            , get
            , remove
            }
