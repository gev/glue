module Reacthome.Assist.Controller.Yandex where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Maybe
import Reacthome.Assist.Dialog.Gate
import Reacthome.Assist.Domain.Answer
import Reacthome.Assist.Domain.Query
import Reacthome.Assist.Service.Dialog
import Reacthome.Gate.Connection.Pool
import Reacthome.Yandex.Dialogs.DialogRequest
import Reacthome.Yandex.Dialogs.DialogResponse
import Reacthome.Yandex.Dialogs.Directives
import Reacthome.Yandex.Dialogs.Request qualified as D
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
    dialog <- fromJSON @DialogRequest ?request
    -- lift $ print dialog
    answer <-
        {-
            TODO: Add timeout
        -}
        lift $
            getAnswer
                Query
                    { message = dialog.request.command
                    , sessionId = dialog.session.session_id
                    }
    toJSON
        DialogResponse
            { response =
                D.Response
                    { text = answer.message
                    , tts = Just answer.message
                    , end_session = fromMaybe False answer.endSession
                    , directives = Just start'account'linking
                    }
            , version = "1.0"
            }
