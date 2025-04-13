module Reacthome.Assist.Controller.Yandex where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.ByteString hiding (head, null)
import Data.Maybe
import Data.UUID hiding (null)
import JOSE.JWT
import JOSE.Payload
import JOSE.PublicKey
import JOSE.Verify (verifySignature)
import Network.HTTP.Types.Header
import Reacthome.Assist.Dialog.Gate
import Reacthome.Assist.Domain.Answer
import Reacthome.Assist.Domain.Query
import Reacthome.Assist.Domain.Server.Id
import Reacthome.Assist.Domain.User
import Reacthome.Assist.Domain.User.Id
import Reacthome.Assist.Domain.Users
import Reacthome.Assist.Service.Dialog
import Reacthome.Gate.Connection.Pool
import Reacthome.Yandex.Dialogs.DialogRequest
import Reacthome.Yandex.Dialogs.DialogRequest.Meta
import Reacthome.Yandex.Dialogs.DialogRequest.Request qualified
import Reacthome.Yandex.Dialogs.DialogRequest.Session
import Reacthome.Yandex.Dialogs.DialogRequest.Session.Application
import Reacthome.Yandex.Dialogs.DialogRequest.Session.User qualified
import Reacthome.Yandex.Dialogs.DialogResponse
import Reacthome.Yandex.Dialogs.DialogResponse.Response qualified as D
import Reacthome.Yandex.Dialogs.DialogResponse.Response.Directives
import Web.Rest
import Web.Rest.Media

runDialog ::
    ( ?answers :: Answers
    , ?gateConnectionPool :: GateConnectionPool
    , ?request :: Request
    , ?publicKeys :: PublicKeys IO
    , ?users :: Users
    ) =>
    ExceptT String IO Response
runDialog = do
    response <- handleE shouldAuthorize run
    toJSON
        DialogResponse
            { response
            , version = "1.0"
            }

run ::
    ( ?answers :: Answers
    , ?gateConnectionPool :: GateConnectionPool
    , ?request :: Request
    , ?publicKeys :: PublicKeys IO
    , ?users :: Users
    ) =>
    ExceptT String IO D.Response
run = do
    user <- getAuthorizedUser
    case user.servers of
        [] -> shouldAddServer user
        (server : _) -> makeAnswer server =<< fromJSON ?request

getAuthorizedUser ::
    ( ?request :: Request
    , ?publicKeys :: PublicKeys IO
    , ?users :: Users
    ) =>
    ExceptT String IO User
getAuthorizedUser = do
    authorization <-
        maybeToExceptT
            "Not authorized"
            . hoistMaybe
            $ ?request.header hAuthorization
    jwt <-
        maybeToExceptT
            "Required `Bearer` authorization"
            . hoistMaybe
            $ stripPrefix "Bearer " authorization
    token <- verifySignature ?publicKeys jwt
    isValid <- lift $ isTokenValidNow token
    if isValid
        then do
            let uid = token.payload.sub
            maybeToExceptT
                ("Unknown user: `" <> toString uid <> "`")
                . ?users.findById
                $ UserId uid
        else throwE "Token is expired"

makeAnswer ::
    ( ?answers :: Answers
    , ?gateConnectionPool :: GateConnectionPool
    ) =>
    ServerId ->
    DialogRequest ->
    ExceptT String IO D.Response
makeAnswer _ LinkingComplete{} =
    pure
        D.Response
            { text = "Отлично!"
            , tts = Just "Отлично!"
            , end_session = False
            , directives = Nothing
            }
makeAnswer server DialogRequest{meta, session, request} = do
    answer <-
        {-
            TODO: Add timeout
        -}
        lift $
            getAnswer
                server
                Query
                    { user_agent = meta.client_id
                    , session = session.session_id
                    , skill = session.skill_id
                    , skill_user = session.user.user_id
                    , skill_application = session.application.application_id
                    , message = request.command
                    }
    pure
        D.Response
            { text = answer.message
            , tts = Just answer.message
            , end_session = fromMaybe False answer.end
            , directives = Nothing
            }

shouldAuthorize :: String -> ExceptT String IO D.Response
shouldAuthorize err = do
    lift $ print err
    pure
        D.Response
            { text = "Привет!"
            , tts = Just "Привет!"
            , end_session = False
            , directives = Just start'account'linking
            }

shouldAddServer :: User -> ExceptT String IO D.Response
shouldAddServer user = do
    lift $
        print
            ( "User: `"
                <> toString user.id.value
                <> "` doesn't have the any known smart home server"
            )
    pure
        D.Response
            { text = "Необходимо добавить сервер умного дома"
            , tts = Just "Необходимо добавить сервер умного дома"
            , end_session = False
            , directives = Nothing
            }
