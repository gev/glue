module Reacthome.Assist.Controller.Yandex where

import Control.Error.Util (exceptT, (??))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, throwE)
import Data.ByteString (stripPrefix)
import Data.Maybe (fromMaybe)
import JOSE.JWT (Token (..), isTokenValidNow)
import JOSE.Payload (Payload (sub))
import JOSE.PublicKey (PublicKeys)
import JOSE.Verify (verifySignature)
import Network.HTTP.Types.Header (hAuthorization)
import Reacthome.Assist.Dialog.Gate (getAnswer)
import Reacthome.Assist.Domain.Answer (Answer (..))
import Reacthome.Assist.Domain.Query (Query (..))
import Reacthome.Assist.Domain.Server.Id (ServerId)
import Reacthome.Assist.Domain.User (User (servers))
import Reacthome.Assist.Domain.User.Id (UserId (..))
import Reacthome.Assist.Domain.Users (Users (..))
import Reacthome.Assist.Service.Dialog (Answers)
import Reacthome.Gate.Connection.Pool (GateConnectionPool)
import Reacthome.Yandex.Dialogs.DialogRequest (DialogRequest (..))
import Reacthome.Yandex.Dialogs.DialogRequest.Meta (Meta (..))
import Reacthome.Yandex.Dialogs.DialogRequest.Request qualified
import Reacthome.Yandex.Dialogs.DialogRequest.Session (Session (..))
import Reacthome.Yandex.Dialogs.DialogRequest.Session.Application (Application (..))
import Reacthome.Yandex.Dialogs.DialogRequest.Session.User qualified
import Reacthome.Yandex.Dialogs.DialogResponse (DialogResponse (..))
import Reacthome.Yandex.Dialogs.DialogResponse.Response qualified as D
import Reacthome.Yandex.Dialogs.DialogResponse.Response.Directives (start'account'linking)
import Rest (Request (..), Response)
import Rest.Media (fromJSON, toJSON)

runDialog ::
    ( ?answers :: Answers
    , ?gateConnectionPool :: GateConnectionPool
    , ?request :: Request
    , ?publicKeys :: PublicKeys IO
    , ?users :: Users
    ) =>
    IO Response
runDialog = exceptT (const $ toJSON shouldAuthorize) toJSON do
    response <-
        getAuthorizedUser >>= \user ->
            case user.servers of
                [] -> pure shouldAddServer
                (server : _) -> do
                    request <- except =<< lift (fromJSON ?request)
                    lift $ makeAnswer server request
    pure
        DialogResponse
            { response
            , version = "1.0"
            }

getAuthorizedUser ::
    ( ?request :: Request
    , ?publicKeys :: PublicKeys IO
    , ?users :: Users
    ) =>
    ExceptT String IO User
getAuthorizedUser = do
    authorization <- ?request.header hAuthorization ?? "Not authorized"
    jwt <- stripPrefix "Bearer " authorization ?? "Required `Bearer` authorization"
    token <- except =<< lift (verifySignature ?publicKeys jwt)
    isValid <- lift $ isTokenValidNow token
    if isValid
        then do
            let uid = token.payload.sub
            except (?users.findById $ UserId uid)
        else throwE "Token is expired"

makeAnswer ::
    ( ?answers :: Answers
    , ?gateConnectionPool :: GateConnectionPool
    ) =>
    ServerId ->
    DialogRequest ->
    IO D.Response
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

shouldAuthorize :: D.Response
shouldAuthorize =
    D.Response
        { text = "Привет!"
        , tts = Just "Привет!"
        , end_session = False
        , directives = Just start'account'linking
        }

shouldAddServer :: D.Response
shouldAddServer =
    D.Response
        { text = "Необходимо добавить сервер умного дома"
        , tts = Just "Необходимо добавить сервер умного дома"
        , end_session = False
        , directives = Nothing
        }
