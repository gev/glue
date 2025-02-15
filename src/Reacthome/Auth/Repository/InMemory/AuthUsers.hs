module Reacthome.Auth.Repository.InMemory.AuthUsers where

import Reacthome.Auth.Environment
import Reacthome.Auth.Repository.InMemory.Challenges
import Reacthome.Auth.Service.AuthUsers
import Reacthome.Auth.Service.Challenges
import Prelude hiding (lookup)

makeAuthUsers :: (?environment :: Environment) => IO AuthUsers
makeAuthUsers = do
    challenges <- makeChallenges
    let
        register = challenges.makeNew
        findBy = challenges.findBy
        remove = challenges.remove
    pure
        AuthUsers
            { register
            , findBy
            , remove
            }
