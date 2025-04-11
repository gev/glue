module Reacthome.Auth.Controller.Users where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.View.Screen.Users
import Web.Rest
import Web.Rest.Media

showUsers ::
    ( ?users :: Users
    ) =>
    ExceptT String IO Response
showUsers =
    toHTML
        <$> users
        =<< lift ?users.getAll
