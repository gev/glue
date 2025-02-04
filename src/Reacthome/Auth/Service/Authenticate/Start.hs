module Reacthome.Auth.Service.Authenticate.Start where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Reacthome.Auth.Domain.Authenticate.Start
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Service.Authenticate.PreAuthenticated
import Reacthome.Auth.Service.Challenges

runStartAuthenticate ::
    ( ?challenges :: Challenges
    , ?users :: Users
    , ?publicKeys :: PublicKeys
    ) =>
    StartAuthenticate ->
    ExceptT String IO PreAuthenticated
runStartAuthenticate command = do
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
