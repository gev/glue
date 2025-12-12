module Reacthome.Auth.Controller.Users where

import Reacthome.Auth.Domain.Users
import Reacthome.Auth.View.Screen.Users
import Rest
import Rest.Media
import Rest.Status (badRequest)

showUsers ::
    (?users :: Users) =>
    IO Response
showUsers =
    ?users.getAll
        >>= either badRequest (toHTML . users)
