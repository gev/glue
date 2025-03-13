module Reacthome.Assist.Controller.Yandex where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.ByteString
import Data.Maybe
import Network.HTTP.Types.Header
import Reacthome.Assist.Dialog.Gate
import Reacthome.Assist.Domain.Answer
import Reacthome.Assist.Domain.Query
import Reacthome.Assist.Service.Dialog
import Reacthome.Gate.Connection.Pool
import Reacthome.Yandex.Dialogs.DialogRequest
import Reacthome.Yandex.Dialogs.DialogResponse
import Reacthome.Yandex.Dialogs.Directives
import Reacthome.Yandex.Dialogs.Request qualified
import Reacthome.Yandex.Dialogs.Response qualified as D
import Reacthome.Yandex.Dialogs.Session
import Web.Rest
import Web.Rest.Media

runDialog ::
    ( ?answers :: Answers
    , ?gateConnectionPool :: GateConnectionPool
    , ?request :: Request
    ) =>
    ExceptT String IO Response
runDialog = do
    response <- maybe shouldAuthorize runDialog' checkAuthorization
    toJSON
        DialogResponse
            { response
            , version = "1.0"
            }

runDialog' ::
    ( ?answers :: Answers
    , ?gateConnectionPool :: GateConnectionPool
    , ?request :: Request
    ) =>
    ByteString ->
    ExceptT String IO D.Response
runDialog' authorization = do
    lift $ print authorization
    dialog <- fromJSON ?request
    lift $ print dialog
    makeAnswer dialog

checkAuthorization :: (?request :: Request) => Maybe ByteString
checkAuthorization =
    ?request.header hAuthorization

shouldAuthorize :: ExceptT String IO D.Response
shouldAuthorize =
    pure
        D.Response
            { text = "Привет!"
            , tts = Just "Привет!"
            , end_session = False
            , directives = Just start'account'linking
            }

makeAnswer ::
    ( ?answers :: Answers
    , ?gateConnectionPool :: GateConnectionPool
    ) =>
    DialogRequest ->
    ExceptT String IO D.Response
makeAnswer LinkingComplete{} =
    pure
        D.Response
            { text = "Отлично!"
            , tts = Just "Отлично!"
            , end_session = False
            , directives = Nothing
            }
makeAnswer DialogRequest{session, request} = do
    answer <-
        {-
            TODO: Add timeout
        -}
        lift $
            getAnswer
                Query
                    { message = request.command
                    , sessionId = session.session_id
                    }
    pure
        D.Response
            { text = answer.message
            , tts = Just answer.message
            , end_session = fromMaybe False answer.endSession
            , directives = Nothing
            }
