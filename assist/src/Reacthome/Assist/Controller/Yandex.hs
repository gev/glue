module Reacthome.Assist.Controller.Yandex where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.ByteString
import Data.Maybe
import Data.Text (Text)
import JOSE.JWT
import JOSE.Payload
import JOSE.PublicKey
import JOSE.Verify (verifySignature)
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
    , ?publicKeys :: PublicKeys IO
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
    ) =>
    ExceptT String IO D.Response
run = do
    user <- getAuthorizedUser
    lift $ print user
    dialog <- fromJSON ?request
    makeAnswer dialog

getAuthorizedUser ::
    ( ?request :: Request
    , ?publicKeys :: PublicKeys IO
    ) =>
    ExceptT String IO Text
getAuthorizedUser = do
    authorization <- maybeToExceptT "Not authorized" . hoistMaybe $ ?request.header hAuthorization
    jwt <- maybeToExceptT "Required `Bearer` authorization" . hoistMaybe $ stripPrefix "Bearer " authorization
    token <- verifySignature ?publicKeys jwt
    isValid <- lift $ isTokenValidNow token
    if isValid
        then pure token.payload.iss
        else throwE "Token is expired"

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
