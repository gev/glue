module Reacthome.Auth.Controller.OAuth where

import Web.Rest
import Web.Rest.Status

oauth ::
    ( ?request :: Request
    , Applicative a
    ) =>
    a Response
oauth = do
    -- print ?request.requestHeader
    redirect mempty "/authentication"
