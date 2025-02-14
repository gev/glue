module Reacthome.Auth.App where

import Data.Function
import Network.Wai
import Network.Wai.Middleware.Static
import Reacthome.Auth.Controller.Authentication.Begin
import Reacthome.Auth.Controller.Authentication.Complete
import Reacthome.Auth.Controller.Registration.Begin
import Reacthome.Auth.Controller.Registration.Complete
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Challenges (Challenges)
import Reacthome.Auth.View.Screen.Authentication
import Reacthome.Auth.View.Screen.Registration
import Util.Rest

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
        \request respond ->
            respond
                =<< ( request & case request.pathInfo of
                        ["authentication"] -> get html authentication
                        ["authentication", "begin"] -> post json beginAuthentication
                        ["authentication", "complete"] -> post json completeAuthentication
                        ["registration"] -> get html registration
                        ["registration", "begin"] -> post json beginRegistration
                        ["registration", "complete"] -> post json completeRegistration
                        _ -> const notFound
                    )
