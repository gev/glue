module Reacthome.Auth.Service.Authentication.Begin where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Reacthome.Auth.Domain.Authentication.Begin
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Service.AuthUsers
import Reacthome.Auth.Service.Authentication.Pre

runBeginAuthentication ::
    ( ?authUsers :: AuthUsers
    , ?users :: Users
    , ?userPublicKeys :: PublicKeys
    ) =>
    BeginAuthentication ->
    ExceptT String IO PreAuthentication
runBeginAuthentication command = do
    user <-
        maybeToExceptT
            ("User with login " <> show command.login.value <> " not found")
            $ ?users.findByLogin command.login
    allowCredentials <- lift $ ?userPublicKeys.findByUserId user.id
    when
        (null allowCredentials)
        $ throwE ("User with login " <> show command.login.value <> " has no any credentials")
    challenge <- lift $ ?authUsers.register user
    pure
        PreAuthentication
            { challenge
            , allowCredentials
            }
