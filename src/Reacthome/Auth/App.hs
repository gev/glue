module Reacthome.Auth.App where

import Control.Monad.Trans.Except
import Data.Aeson
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Static
import Reacthome.Auth.Controller.Authentication.Begin
import Reacthome.Auth.Controller.Authentication.Complete
import Reacthome.Auth.Controller.Registration.Begin
import Reacthome.Auth.Controller.Registration.Complete
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Challenges
import Util.Wai

app ::
    ( ?environment :: Environment
    , ?challenges :: Challenges
    , ?users :: Users
    , ?publicKeys :: PublicKeys
    ) =>
    Application
app req respond
    | req.requestMethod == methodGet
        || req.requestMethod == methodHead =
        static req respond
    | req.requestMethod == methodPost = do
        let respond' ::
                (FromJSON req, ToJSON res) =>
                (req -> ExceptT String IO res) ->
                IO ResponseReceived
            respond' = makeRespond req respond
        case req.pathInfo of
            ["registration", "begin"] -> respond' beginRegistration
            ["registration", "complete"] -> respond' completeRegistration
            ["authentication", "begin"] -> respond' beginAuthentication
            ["authentication", "complete"] -> respond' completeAuthentication
            _ -> respond notAllowed
    | otherwise = respond notAllowed

static :: Application
static = staticApp (defaultFileServerSettings "public")
