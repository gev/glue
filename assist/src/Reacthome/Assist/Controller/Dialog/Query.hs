module Reacthome.Assist.Controller.Dialog.Query where

import Data.Aeson (encode)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Reacthome.Assist.Domain.Query (Query)
import Reacthome.Assist.Domain.Server.Id (ServerId (..))
import Reacthome.Assist.Service.Dialog (Container)
import Reacthome.Gate.Connection (GateConnection (..))
import Reacthome.Gate.Connection.Pool (GateConnectionPool (..))

sendQuery ::
    (?gateConnectionPool :: GateConnectionPool) =>
    ServerId ->
    Container Query ->
    IO ()
sendQuery sid query = do
    gate <- ?gateConnectionPool.getConnection sid.value
    gate.send (decodeUtf8 . encode $ query)
