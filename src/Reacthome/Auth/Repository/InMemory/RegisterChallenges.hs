module Reacthome.Auth.Repository.InMemory.RegisterChallenges where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Challenge
import Reacthome.Auth.Service.Register.Challenges
import Util.MVar
import Prelude hiding (lookup)

mkRegisterChallenges :: (?environment :: Environment) => IO RegisterChallenges
mkRegisterChallenges = do
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
        RegisterChallenges
            { register
            , findBy
            , remove
            }
