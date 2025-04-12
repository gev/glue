module Reacthome.Assist.App where

import JOSE.PublicKey
import Network.Wai
import Reacthome.Assist.Controller.Yandex
import Reacthome.Assist.Domain.Users
import Reacthome.Assist.Service.Dialog
import Reacthome.Gate.Connection.Pool
import Web.Rest
import Web.Rest.Method
import Web.Rest.Status

app ::
    ( ?answers :: Answers
    , ?gateConnectionPool :: GateConnectionPool
    , ?publicKeys :: PublicKeys IO
    , ?users :: Users
    ) =>
    Application
app request respond = do
    let ?request = rest request
    respond
        =<< case request.pathInfo of
            ["yandex"] -> post runDialog
            _ -> notFound
