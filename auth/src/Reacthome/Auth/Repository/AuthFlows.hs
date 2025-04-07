module Reacthome.Auth.Repository.AuthFlows where

import Reacthome.Auth.Domain.Challenges
import Reacthome.Auth.Environment
import Reacthome.Auth.Repository.Challenges
import Reacthome.Auth.Service.AuthFlows

makeAuthFlows :: (?environment :: Environment) => IO AuthFlows
makeAuthFlows = do
    challenges <- makeChallenges
    let
        start = challenges.makeNew ?environment.authFlowCookieTTL
        findBy = challenges.findBy
        stop = challenges.remove
    pure
        AuthFlows
            { start
            , findBy
            , stop
            }
