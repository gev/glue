module Reacthome.Relay.Relay where

import Data.Hashable (Hashable, hashWithSalt)
import Data.Text (show)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Reacthome.Relay.Message (RelayMessage, parseMessage, serializeMessage)
import Web.WebSockets.Connection (WebSocketConnection (..))
import Prelude hiding (show)

data Relay = Relay
    { uid :: UUID
    , receiveMessage :: IO RelayMessage
    , sendMessage :: RelayMessage -> IO ()
    , close :: IO ()
    }

instance Eq Relay where
    r1 == r2 = r1.uid == r2.uid

instance Hashable Relay where
    hashWithSalt sault r = hashWithSalt sault r.uid

makeRelay :: UUID -> WebSocketConnection -> Relay
makeRelay uid connection =
    let
        receiveMessage = parseMessage <$> connection.receiveMessage
        sendMessage = connection.sendMessage . serializeMessage
        close = connection.close $ "Close relay" <> show uid
     in
        Relay{..}

newRelay :: WebSocketConnection -> IO Relay
newRelay connection = do
    uid <- nextRandom
    pure $ makeRelay uid connection
