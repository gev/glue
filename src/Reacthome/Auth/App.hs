module Reacthome.Auth.App where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Static
import Reacthome.Auth.Environment

-- import Reacthome.Auth.Controller.Authenticate.Finish
-- import Reacthome.Auth.Controller.Authenticate.Start

import Reacthome.Auth.Controller.Register.Finish
import Reacthome.Auth.Controller.Register.Start
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Service.Register.Challenges
import Util.Wai

app ::
    ( ?environment :: Environment
    , ?challenges :: RegisterChallenges
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
                (req -> IO (Either String res)) ->
                IO ResponseReceived
            respond' = mkRespond req respond
        case req.pathInfo of
            ["register", "start"] -> respond' startRegister
            ["register", "finish"] -> respond' finishRegister
            -- ["authenticate", "start"] -> respond' startAuthenticate
            -- ["authenticate", "finish"] -> respond' finishAuthenticate
            _ -> respond notAllowed
    | otherwise = respond notAllowed

static :: Application
static = staticApp (defaultFileServerSettings "public")
