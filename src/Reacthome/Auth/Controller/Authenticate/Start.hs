module Reacthome.Auth.Controller.Authenticate.Start where

import Control.Monad.Trans.Except
import Reacthome.Auth.Controller.Authenticate.AuthenticateOptions
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialRequestOptions
import Reacthome.Auth.Domain.Authenticate.Start
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Authenticate.Start
import Reacthome.Auth.Service.Challenges

startAuthenticate ::
    ( ?environment :: Environment
    , ?challenges :: Challenges
    , ?users :: Users
    , ?publicKeys :: PublicKeys
    ) =>
    AuthenticateOptions ->
    ExceptT String IO PublicKeyCredentialRequestOptions
startAuthenticate options = do
    login <- mkUserLogin options.login
    mkPublicKeyCredentialRequestOptions
        <$> runStartAuthenticate
            StartAuthenticate
                { login
                }
