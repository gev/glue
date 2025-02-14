module Reacthome.Auth.App where

import Data.Function
import Network.HTTP.Types
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
        \request respond -> do
            respond
                =<< ( request & case request.pathInfo of
                        [] -> get html authentication
                        ["register"] -> get html registration
                        ["registration", "begin"] -> post json beginRegistration
                        ["registration", "complete"] -> post json completeRegistration
                        ["authentication", "begin"] -> post json beginAuthentication
                        ["authentication", "complete"] -> post json completeAuthentication
                        _ -> const notFound
                    )

type Controller a t =
    (Applicative a) =>
    (t -> Request -> a Response) ->
    t ->
    Request ->
    a Response

get :: Controller a t
get = ifMethod methodGet

post :: Controller a t
post = ifMethod methodPost

ifMethod :: Method -> Controller a t
ifMethod method media runController request =
    if request.requestMethod == method
        then media runController request
        else notAllowed method
