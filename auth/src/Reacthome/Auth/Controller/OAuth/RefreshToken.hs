module Reacthome.Auth.Controller.OAuth.RefreshToken where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import JOSE.KeyPair
import Reacthome.Auth.Controller.OAuth.Grant
import Reacthome.Auth.Controller.OAuth.Token
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Challenge
import Reacthome.Auth.Service.RefreshTokens
import Web.Rest
import Web.Rest.Media

refreshToken ::
    ( ?environment :: Environment
    , ?request :: Request
    , ?refreshTokens :: RefreshTokens
    , ?keyPair :: KeyPair
    ) =>
    ExceptT String IO Response
refreshToken = do
    token <- makeChallenge <$> getRefreshToken
    user <- maybeToExceptT "Invalid refresh token" $ ?refreshTokens.findBy token
    lift $ ?refreshTokens.remove token
    toJSON =<< lift (generateToken user)

{-
    TODO: What I should response on the error?
-}
