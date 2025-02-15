module Reacthome.Auth.Repository.InMemory.AuthFlows where

import Reacthome.Auth.Environment
import Reacthome.Auth.Repository.InMemory.Challenges
import Reacthome.Auth.Service.AuthFlow
import Reacthome.Auth.Service.AuthFlows
import Reacthome.Auth.Service.Challenges

makeAuthFlows :: (?environment :: Environment) => IO AuthFlows
makeAuthFlows = do
    challenges <- makeChallenges
    let
        startCredentialGrantFlow =
            challenges.makeNew CredentialGrant

        startAuthCodeGrantFlow scope state redirectUri clientId =
            challenges.makeNew $ AuthCodeGrant scope state redirectUri clientId

        findBy = challenges.findBy

        stop = challenges.remove

    pure
        AuthFlows
            { startCredentialGrantFlow
            , startAuthCodeGrantFlow
            , findBy
            , stop
            }
