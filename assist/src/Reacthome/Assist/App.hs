module Reacthome.Assist.App where

import Network.Wai
import Reacthome.Assist.Controller.Yandex
import Web.Rest
import Web.Rest.Method
import Web.Rest.Status

app :: Application
app request respond = do
    let ?request = rest request
    respond
        =<< case request.pathInfo of
            ["yandex"] -> post runDialog
            _ -> notFound
