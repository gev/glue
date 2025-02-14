module Reacthome.Auth.Repository.InMemory.AuthUsers where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.AuthUsers
import Reacthome.Auth.Service.Challenge
import Util.MVar
import Prelude hiding (lookup)

makeAuthUsers :: (?environment :: Environment) => IO AuthUsers
makeAuthUsers = do
    map' <- newMVar empty
    let
        register options = do
            challenge <- makeRandomChallenge ?environment.challengeSize
            runModify map' $ insert challenge options
            void $ forkIO do
                threadDelay $ 1_000 * ?environment.timeout
                remove challenge
            pure challenge

        findBy = MaybeT . runRead map' . lookup

        remove = runModify map' . delete

    pure
        AuthUsers
            { register
            , findBy
            , remove
            }
