module Reacthome.Auth.App where

import JOSE.KeyPair
import Network.Wai
import Reacthome.Auth.Controller.Authentication
import Reacthome.Auth.Controller.Authentication.Begin
import Reacthome.Auth.Controller.Authentication.Complete
import Reacthome.Auth.Controller.OAuth
import Reacthome.Auth.Controller.OAuth.ExchangeCodeForToken
import Reacthome.Auth.Controller.OAuth.RefreshToken
import Reacthome.Auth.Controller.Registration
import Reacthome.Auth.Controller.Registration.Begin
import Reacthome.Auth.Controller.Registration.Complete
import Reacthome.Auth.Controller.WellKnown.JWKS
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.AuthFlows
import Reacthome.Auth.Service.AuthUsers
import Web.Rest
import Web.Rest.Method
import Web.Rest.Status

app ::
    ( ?environment :: Environment
    , ?authFlows :: AuthFlows
    , ?authUsers :: AuthUsers
    , ?users :: Users
    , ?publicKeys :: PublicKeys
    , ?keyPair :: KeyPair
    ) =>
    Application
app request respond = do
    let ?request = rest request
    respond
        =<< case request.pathInfo of
            [] -> get oauth
            ["token"] -> post exchangeCodeForToken
            ["refresh"] -> post refreshToken
            ["authentication"] -> get showAuthentication
            ["authentication", "begin"] -> post beginAuthentication
            ["authentication", "complete"] -> post completeAuthentication
            ["registration"] -> get showRegistration
            ["registration", "begin"] -> post beginRegistration
            ["registration", "complete"] -> post completeRegistration
            [".well-known", "jwks.json"] -> get jwks
            _ -> notFound
