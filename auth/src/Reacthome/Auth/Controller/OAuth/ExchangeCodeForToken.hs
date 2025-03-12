module Reacthome.Auth.Controller.OAuth.ExchangeCodeForToken where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import JOSE.KeyPair
import Reacthome.Auth.Controller.OAuth.Grant
import Reacthome.Auth.Controller.OAuth.Token
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.AuthUsers
import Reacthome.Auth.Service.Challenge
import Reacthome.Auth.Service.RefreshTokens
import Web.Rest
import Web.Rest.Media

exchangeCodeForToken ::
    ( ?environment :: Environment
    , ?request :: Request
    , ?authUsers :: AuthUsers
    , ?refreshTokens :: RefreshTokens
    , ?keyPair :: KeyPair
    ) =>
    ExceptT String IO Response
exchangeCodeForToken = do
    code <- makeChallenge <$> getAuthorizationCode
    user <- maybeToExceptT "Invalid exchange code" $ ?authUsers.findBy code
    lift $ ?authUsers.remove code
    toJSON =<< lift (generateToken user)

{-
    TODO: What I should response on the error?
-}
