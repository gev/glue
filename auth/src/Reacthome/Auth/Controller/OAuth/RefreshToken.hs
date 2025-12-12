module Reacthome.Auth.Controller.OAuth.RefreshToken where

import Control.Error.Util (exceptT)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
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
import Rest.Status (badRequest)

refreshToken ::
    ( ?environment :: Environment
    , ?request :: Request
    , ?clients :: Clients
    , ?refreshTokens :: RefreshTokens
    , ?keyPair :: KeyPair
    ) =>
    IO Response
refreshToken = exceptT badRequest toJSON do
    hash <- makeHash <$> getRefreshToken
    token <- except =<< lift (?refreshTokens.findByHash hash)
    except =<< lift (?refreshTokens.remove token)
    generateToken token.userId

{-
    TODO: What I should response on the error?
    TODO: Move this logic out of the controller
-}
