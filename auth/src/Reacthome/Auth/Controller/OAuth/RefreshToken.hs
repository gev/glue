module Reacthome.Auth.Controller.OAuth.RefreshToken where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.ByteString.Base64.URL
import JOSE.KeyPair
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
    params <- ?request.bodyParams

    lift $ print ?request.headers
    lift $ print params.list

    grant_type <- params.lookup "grant_type"
    when (grant_type /= "refresh_token") $
        throwE "Invalid `grant_type`"

    client_id <- params.lookup "client_id"
    client_secret <- params.lookup "client_secret"
    when (client_id /= "reacthome" && client_secret /= "reacthome") $
        throwE "Invalid client credentials"

    refresh_token <- except . decodeUnpadded =<< params.lookup "refresh_token"
    user <-
        maybeToExceptT "Invalid refresh token"
            . ?authUsers.findBy
            $ makeChallenge refresh_token

    lift $ print @String "refreshToken"

    toJSON =<< lift (generateToken user)

{-
    TODO: What I should response on the error?
-}
