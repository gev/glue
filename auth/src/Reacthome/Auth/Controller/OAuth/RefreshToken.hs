module Reacthome.Auth.Controller.OAuth.RefreshToken where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import JOSE.KeyPair
import Reacthome.Auth.Controller.OAuth.Grant
import Reacthome.Auth.Controller.OAuth.Token
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.AuthUsers
import Reacthome.Auth.Service.Challenge
import Web.Rest
import Web.Rest.Media

refreshToken ::
    ( ?environment :: Environment
    , ?request :: Request
    , ?authUsers :: AuthUsers
    , ?keyPair :: KeyPair
    ) =>
    ExceptT String IO Response
refreshToken = do
    user <-
        maybeToExceptT "Invalid refresh token"
            . ?authUsers.findBy
            . makeChallenge
            =<< getRefreshToken
    toJSON =<< lift (generateToken user)

{-
    TODO: What I should response on the error?
-}
