module Reacthome.Auth.Controller.OAuth.Token where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Web.Rest

exchangeCodeForToken ::
    ( ?request :: Request
    ) =>
    ExceptT String IO Response
exchangeCodeForToken = do
    lift $ print ?request.headers
    lift $ print =<< ?request.body
    throwE "Not implemented"
