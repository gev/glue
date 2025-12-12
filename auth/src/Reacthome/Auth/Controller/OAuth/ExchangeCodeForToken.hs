module Reacthome.Auth.Controller.OAuth.ExchangeCodeForToken where

import Control.Error.Util (exceptT)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import JOSE.KeyPair
import Reacthome.Auth.Controller.OAuth.Grant (getAuthorizationCode)
import Reacthome.Auth.Controller.OAuth.Token
import Reacthome.Auth.Domain.Challenge
import Reacthome.Auth.Domain.Clients
import Reacthome.Auth.Domain.RefreshTokens
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.AuthUsers
import Rest
import Rest.Media
import Rest.Status (badRequest)

exchangeCodeForToken ::
    ( ?environment :: Environment
    , ?request :: Request
    , ?authUsers :: AuthUsers
    , ?clients :: Clients
    , ?refreshTokens :: RefreshTokens
    , ?keyPair :: KeyPair
    ) =>
    IO Response
exchangeCodeForToken = exceptT badRequest toJSON do
    code <- makeChallenge <$> getAuthorizationCode
    user <- except =<< lift (?authUsers.findBy code)
    lift $ ?authUsers.remove code
    generateToken user.id

{-
    TODO: What I should response on the error?
-}
