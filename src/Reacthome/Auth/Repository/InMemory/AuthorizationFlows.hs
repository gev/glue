module Reacthome.Auth.Repository.InMemory.AuthorizationFlows where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.AuthorizationFlow
import Reacthome.Auth.Service.AuthorizationFlows
import Reacthome.Auth.Service.Challenge
import Util.MVar
import Prelude hiding (lookup)

makeAuthorizationFlows :: (?environment :: Environment) => IO AuthorizationFlows
makeAuthorizationFlows = do
    map' <- newMVar empty
    let
        startFlow flow = do
            challenge <- makeRandomChallenge ?environment.challengeSize
            runModify map' $ insert challenge flow
            void $ forkIO do
                threadDelay $ 1_000 * ?environment.timeout
                stop challenge
            pure challenge

        startCredentialGrantFlow =
            startFlow CredentialGrant

        startAuthorizationCodeGrantFlow scope state redirectUri clientId =
            startFlow $ AuthorizationCodeGrant scope state redirectUri clientId

        findBy = MaybeT . runRead map' . lookup

        stop = runModify map' . delete

    pure
        AuthorizationFlows
            { startCredentialGrantFlow
            , startAuthorizationCodeGrantFlow
            , findBy
            , stop
            }
