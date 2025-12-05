module Reacthome.Auth.Controller.OAuth.RefreshToken where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import JOSE.KeyPair
import Reacthome.Auth.Controller.OAuth.Grant
import Reacthome.Auth.Controller.OAuth.Token
import Reacthome.Auth.Domain.Clients
import Reacthome.Auth.Domain.Hash
import Reacthome.Auth.Domain.RefreshToken
import Reacthome.Auth.Domain.RefreshTokens
import Reacthome.Auth.Environment
import Rest
import Rest.Media

refreshToken ::
    ( ?environment :: Environment
    , ?request :: Request
    , ?clients :: Clients
    , ?refreshTokens :: RefreshTokens
    , ?keyPair :: KeyPair
    ) =>
    ExceptT String IO Response
refreshToken = do
    hash <- makeHash <$> getRefreshToken
    token <- maybeToExceptT "Invalid refresh token" $ ?refreshTokens.findByHash hash
    lift $ ?refreshTokens.remove token
    toJSON =<< generateToken token.userId

{-
    TODO: What I should response on the error?
    TODO: Move this logic out of the controller
-}
