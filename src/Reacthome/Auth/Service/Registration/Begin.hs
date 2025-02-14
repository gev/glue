module Reacthome.Auth.Service.Registration.Begin where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Reacthome.Auth.Domain.Registration.Begin
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.User.Id
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Service.AuthUsers
import Reacthome.Auth.Service.Registration.Pre

runBeginRegistration ::
    ( ?authUsers :: AuthUsers
    , ?users :: Users
    ) =>
    BeginRegistration ->
    ExceptT String IO PreRegistration
runBeginRegistration command = do
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
            challenge <- lift $ ?authUsers.register user
            pure $ PreRegistration user challenge
