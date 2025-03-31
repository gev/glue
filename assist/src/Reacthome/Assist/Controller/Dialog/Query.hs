module Reacthome.Assist.Controller.Dialog.Query where

import Data.Aeson
import Data.Maybe
import Data.Text.Lazy.Encoding
import Data.UUID
import Reacthome.Assist.Domain.Query
import Reacthome.Assist.Service.Dialog
import Reacthome.Gate.Connection
import Reacthome.Gate.Connection.Pool

sendQuery ::
    ( ?gateConnectionPool :: GateConnectionPool
    ) =>
    Container Query ->
    IO ()
sendQuery query = do
    gate <- ?gateConnectionPool.getConnection npDaemon
    gate.send (decodeUtf8 . encode $ query)

myDaemon :: UUID
myDaemon = fromJust $ fromString "02aaee3f-a050-43d5-bbf2-e0f2abd73a6e"

npDaemon :: UUID
npDaemon = fromJust $ fromString "4d6f75da-4fc7-424f-93fd-266d7c300417"
