module Reacthome.Auth.Controller.OAuth.ExchangeCodeForToken where

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

exchangeCodeForToken ::
    ( ?environment :: Environment
    , ?request :: Request
    , ?authUsers :: AuthUsers
    , ?keyPair :: KeyPair
    ) =>
    ExceptT String IO Response
exchangeCodeForToken = do
    params <- ?request.bodyParams

    grant_type <- params.lookup "grant_type"
    when (grant_type /= "authorization_code") $
        throwE "Invalid `grant_type`"

    client_id <- params.lookup "client_id"
    client_secret <- params.lookup "client_secret"
    when (client_id /= "reacthome" && client_secret /= "reacthome") $
        throwE "Invalid client credentials"

    code <- except . decodeUnpadded =<< params.lookup "code"
    user <-
        maybeToExceptT "Invalid exchange code"
            . ?authUsers.findBy
            $ makeChallenge code

    lift $ print @String "exchangeCodeForToken"

    toJSON =<< lift (generateToken user)

{-
    TODO: What I should response on the error?
-}
