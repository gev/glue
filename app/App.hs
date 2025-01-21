module App where

import Data.Aeson
import Environment
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Static

-- import Service.Authenticate.Finish
-- import Service.Authenticate.Start
import Service.Register.Finish
import Service.Register.Start
import Util.Wai

app :: (?environment :: Environment) => Application
app req respond
    | req.requestMethod == methodGet
        || req.requestMethod == methodHead =
        static req respond
    | req.requestMethod == methodPost = do
        let contentType = lookup hContentType req.requestHeaders
        if contentType == Just ctApplicationJson
            then do
                let respond' ::
                        (FromJSON req, ToJSON res) =>
                        (req -> IO (Maybe res)) ->
                        IO ResponseReceived
                    respond' = mkRespond req respond
                case req.pathInfo of
                    ["register", "start"] -> respond' startRegister
                    ["register", "finish"] -> respond' finishRegister
                    -- ["authenticate", "start"] -> respond' startAuthenticate
                    -- ["authenticate", "finish"] -> respond' finishAuthenticate
                    _ -> respond notAllowed
            else respond notAllowed
    | otherwise = respond notAllowed

static :: Application
static = staticApp (defaultFileServerSettings "public")
