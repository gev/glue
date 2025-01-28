module Repository.InMemory.RegisterChallenges where

import Control.Concurrent
import Control.Monad
import Data.HashMap.Strict
import Environment
import Service.Challenge
import Service.Register.Challenges
import Util.MVar
import Prelude hiding (lookup)

mkRegisterChallenges :: (?environment :: Environment) => IO RegisterChallenges
mkRegisterChallenges = do
    set <- newMVar empty
    let get k = runRead set $ lookup k
        remove k = runModify set $ delete k
        register options = do
            challenge <- mkRandomChallenge ?environment.challengeSize
            runModify set $ insert challenge options
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
