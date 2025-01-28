module Repository.InMemory.RegisterChallenges where

import Control.Concurrent
import Control.Monad
import Data.HashSet
import Service.Challenge
import Service.Register.Challenges
import Util.MVar

mkRegisterChallenges :: Int -> IO RegisterChallenges
mkRegisterChallenges timeout = do
    set <- newMVar empty
    let has k = runRead set $ member k
        remove k = runModify set $ delete k
        get = do
            challenge <- mkRandomChallenge 20
            runModify set $ insert challenge
            void $ forkIO do
                threadDelay $ 1_000 * timeout
                remove challenge
            pure challenge
    pure
        RegisterChallenges
            { has
            , get
            , remove
            }
