module Reacthome.Auth.Controller.Authentication.Begin where

import Control.Monad.Trans.Except
import Reacthome.Auth.Controller.Authentication.Options
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialRequestOptions
import Reacthome.Auth.Domain.Authentication.Begin
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.AuthUsers
import Reacthome.Auth.Service.Authentication.Begin
import Web.Rest
import Web.Rest.Media

beginAuthentication ::
    ( ?request :: Request
    , ?environment :: Environment
    , ?authUsers :: AuthUsers
    , ?users :: Users
    , ?publicKeys :: PublicKeys
    ) =>
    ExceptT String IO Response
beginAuthentication = do
    options <- fromJSON @AuthenticationOptions ?request
    login <- makeUserLogin options.login
    toJSON . makePublicKeyCredentialRequestOptions
        =<< runBeginAuthentication
            BeginAuthentication
                { login
                }
