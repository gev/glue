module Reacthome.Auth.Service.Register.Start where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Reacthome.Auth.Domain.Register.Start
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.User.Id
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Service.Register.Challenges
import Reacthome.Auth.Service.Register.PreRegistered

runStartRegister ::
    ( ?challenges :: RegisterChallenges
    , ?users :: Users
    ) =>
    StartRegister ->
    ExceptT String IO PreRegistered
runStartRegister command = do
    isUserExists <- lift $ ?users.has command.login
    if isUserExists
        then
            throwE $
                "User with login "
                    <> show command.login.value
                    <> " already exists"
        else do
            uid <- lift mkRandomUserId
            let user = mkNewUser uid command.login command.name
            challenge <- lift $ ?challenges.register user
            pure $ PreRegistered user challenge
