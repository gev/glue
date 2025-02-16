module Reacthome.Auth.Controller.OAuth.Refresh where

import Control.Monad.Trans.Except
import Web.Rest
import Web.Rest.Status

refreshToken ::
    ( ?request :: Request
    , Monad m
    ) =>
    m Response
refreshToken =
    either badRequest pure =<< runExceptT do
        throwE "Not implemented"
