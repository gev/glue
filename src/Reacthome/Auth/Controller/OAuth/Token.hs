module Reacthome.Auth.Controller.OAuth.Token where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Web.Rest
import Web.Rest.Status

exchangeCodeForToken ::
    ( ?request :: Request
    ) =>
    IO Response
exchangeCodeForToken =
    either badRequest pure =<< runExceptT do
        lift $ print ?request.headers
        throwE "Not implemented"
