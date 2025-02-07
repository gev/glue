module Reacthome.Auth.Controller.Authentication.Begin where

import Control.Monad.Trans.Except
import Reacthome.Auth.Controller.Authentication.Options
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialRequestOptions
import Reacthome.Auth.Domain.Authentication.Begin
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Authentication.Begin
import Reacthome.Auth.Service.Challenges

beginAuthentication ::
    ( ?environment :: Environment
    , ?challenges :: Challenges
    , ?users :: Users
    , ?publicKeys :: PublicKeys
    ) =>
    AuthenticationOptions ->
    ExceptT String IO PublicKeyCredentialRequestOptions
beginAuthentication options = do
    login <- makeUserLogin options.login
    makePublicKeyCredentialRequestOptions
        <$> runBeginAuthentication
            BeginAuthentication
                { login
                }
