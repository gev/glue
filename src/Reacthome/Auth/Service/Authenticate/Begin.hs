module Reacthome.Auth.Service.Authenticate.Begin where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Reacthome.Auth.Domain.Authenticate.Begin
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Service.Authenticate.PreAuthenticated
import Reacthome.Auth.Service.Challenges

runBeginAuthenticate ::
    ( ?challenges :: Challenges
    , ?users :: Users
    , ?publicKeys :: PublicKeys
    ) =>
    BeginAuthenticate ->
    ExceptT String IO PreAuthenticated
runBeginAuthenticate command = do
    user <-
        maybeToExceptT
            ("User with login " <> show command.login.value <> " not found")
            $ ?users.findByLogin command.login
    allowCredentials <-
        maybeToExceptT
            ("User with login " <> show command.login.value <> " has no any credentials")
            $ ?publicKeys.findByUserId user.id
    challenge <- lift $ ?challenges.register user
    pure
        PreAuthenticated
            { challenge
            , allowCredentials
            }
