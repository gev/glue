module Reacthome.Auth.App where

import Network.Wai
import Network.Wai.Middleware.Static
import Reacthome.Auth.Controller.Authentication
import Reacthome.Auth.Controller.Authentication.Begin
import Reacthome.Auth.Controller.Authentication.Complete
import Reacthome.Auth.Controller.Registration
import Reacthome.Auth.Controller.Registration.Begin
import Reacthome.Auth.Controller.Registration.Complete
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Challenges
import Reacthome.OAuth2.Controller.OAuth
import Web.Rest
import Web.Rest.Method
import Web.Rest.Status

app ::
    ( ?environment :: Environment
    , ?challenges :: Challenges
    , ?users :: Users
    , ?publicKeys :: PublicKeys
    ) =>
    Application
app =
    staticPolicy
        (addBase "public")
        \request respond -> do
            let ?rest = rest request
            respond
                =<< case request.pathInfo of
                    [] -> get oauth
                    ["authentication"] -> get showAuthentication
                    ["authentication", "begin"] -> post beginAuthentication
                    ["authentication", "complete"] -> post completeAuthentication
                    ["registration"] -> get showRegistration
                    ["registration", "begin"] -> post beginRegistration
                    ["registration", "complete"] -> post completeRegistration
                    _ -> notFound
