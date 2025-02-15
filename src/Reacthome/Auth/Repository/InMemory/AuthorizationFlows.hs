module Reacthome.Auth.Repository.InMemory.AuthorizationFlows where

import Reacthome.Auth.Environment
import Reacthome.Auth.Repository.InMemory.Challenges
import Reacthome.Auth.Service.AuthorizationFlow
import Reacthome.Auth.Service.AuthorizationFlows
import Reacthome.Auth.Service.Challenges

makeAuthorizationFlows :: (?environment :: Environment) => IO AuthorizationFlows
makeAuthorizationFlows = do
    challenges <- makeChallenges
    let
        startCredentialGrantFlow =
            challenges.makeNew CredentialGrant

        startAuthorizationCodeGrantFlow scope state redirectUri clientId =
            challenges.makeNew $ AuthorizationCodeGrant scope state redirectUri clientId

        findBy = challenges.findBy

        stop = challenges.remove

    pure
        AuthorizationFlows
            { startCredentialGrantFlow
            , startAuthorizationCodeGrantFlow
            , findBy
            , stop
            }
