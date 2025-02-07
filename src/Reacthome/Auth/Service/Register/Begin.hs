module Reacthome.Auth.Service.Register.Begin where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Reacthome.Auth.Domain.Register.Begin
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.User.Id
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Service.Challenges
import Reacthome.Auth.Service.Register.PreRegistered

runBeginRegister ::
    ( ?challenges :: Challenges
    , ?users :: Users
    ) =>
    BeginRegister ->
    ExceptT String IO PreRegistered
runBeginRegister command = do
    isUserExists <- lift $ ?users.has command.login
    if isUserExists
        then
            throwE $
                "User with login "
                    <> show command.login.value
                    <> " already exists"
        else do
            uid <- lift makeRandomUserId
            let user = makeNewUser uid command.login command.name
            challenge <- lift $ ?challenges.register user
            pure $ PreRegistered user challenge
