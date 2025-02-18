module Reacthome.Assist.Controller.Yandex where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Reacthome.Yandex.Dialogs.DialogRequest
import Reacthome.Yandex.Dialogs.DialogResponse
import Reacthome.Yandex.Dialogs.Directives
import Reacthome.Yandex.Dialogs.Request qualified as D
import Reacthome.Yandex.Dialogs.Response qualified as D
import Web.Rest
import Web.Rest.Media

runDialog ::
    (?request :: Request) =>
    ExceptT String IO Response
runDialog = do
    dialog <- fromJSON @DialogRequest ?request
    let greeting = dialog.request.command
    -- lift $ print dialog
    toJSON
        DialogResponse
            { response =
                D.Response
                    { text = greeting
                    , tts = Just greeting
                    , end_session = False
                    , directives = Just start'account'linking
                    }
            , version = "1.0"
            }
