module Reacthome.Auth.Repository.AuthUsers where

import Reacthome.Auth.Domain.Challenges
import Reacthome.Auth.Environment
import Reacthome.Auth.Repository.Challenges
import Reacthome.Auth.Service.AuthUsers
import Prelude hiding (lookup)

makeAuthUsers :: (?environment :: Environment) => IO AuthUsers
makeAuthUsers = do
    challenges <- makeChallenges
    let
        register = challenges.makeNew ?environment.authTimeout
        findBy = challenges.findBy
        remove = challenges.remove
    pure AuthUsers{..}
