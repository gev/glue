module Reacthome.Assist.Controller.Yandex where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Reacthome.Yandex.Dialogs.DialogRequest
import Reacthome.Yandex.Dialogs.DialogResponse
import Reacthome.Yandex.Dialogs.Request
import Reacthome.Yandex.Dialogs.Response

-- import Reacthome.Yandex.Dialogs.Request

runDialog ::
    DialogRequest ->
    ExceptT String IO DialogResponse
runDialog dialog = do
    let greeting = dialog.request.command
    lift $ print dialog
    pure
        DialogResponse
            { response =
                Response
                    { text = greeting
                    , tts = Just greeting
                    , end_session = False
                    , directives = Nothing -- Just start'account'linking
                    }
            , version = "1.0"
            }
