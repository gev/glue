module Reacthome.Assist.Controller.Dialog.Query where

import Data.Aeson
import Data.Text.Lazy.Encoding
import Reacthome.Assist.Domain.Query
import Reacthome.Assist.Domain.Server.Id
import Reacthome.Assist.Service.Dialog
import Reacthome.Gate.Connection
import Reacthome.Gate.Connection.Pool

sendQuery ::
    ( ?gateConnectionPool :: GateConnectionPool
    ) =>
    ServerId ->
    Container Query ->
    IO ()
sendQuery sid query = do
    gate <- ?gateConnectionPool.getConnection sid.value
    gate.send (decodeUtf8 . encode $ query)
