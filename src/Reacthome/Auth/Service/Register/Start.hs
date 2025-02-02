module Reacthome.Auth.Service.Register.Start where

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
    StartRegisterUser ->
    IO (Either String PreRegistered)
runStartRegister command = do
    isUserExists <- ?users.has command.login
    if isUserExists
        then
            pure . Left $
                "User with login "
                    <> show command.login.value
                    <> " already exists"
        else do
            uid <- mkRandomUserId
            let user = mkNewUser uid command.login command.name
            challenge <- ?challenges.register user
            pure . Right $ PreRegistered user challenge
