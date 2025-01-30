module Reacthome.Auth.Repository.InMemory.RegisterChallenges where

import Control.Concurrent
import Control.Monad
import Data.HashMap.Strict
import Reacthome.Auth.Controller.Challenge
import Reacthome.Auth.Controller.Register.Challenges
import Reacthome.Auth.Environment
import Util.MVar
import Prelude hiding (lookup)

mkRegisterChallenges :: (?environment :: Environment) => IO RegisterChallenges
mkRegisterChallenges = do
    map' <- newMVar empty
    let get = runRead map' . lookup
        remove = runModify map' . delete
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
