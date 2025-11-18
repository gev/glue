module Reacthome.Relay.Relay where

import Data.ByteString (toStrict)
import Data.Hashable (Hashable, hashWithSalt)
import Data.Text (show)
import Data.UUID (toByteString)
import Data.Word (Word64)
import Reacthome.Relay.Message (RelayMessage (..), parseMessage, serializeMessage)
import Web.WebSockets.Connection (WebSocketConnection (..))
import Prelude hiding (show)

data Relay = Relay
    { uid :: !Word64
    , receiveMessage :: IO RelayMessage
    , sendMessage :: RelayMessage -> IO ()
    , close :: IO ()
    }

instance Eq Relay where
    r1 == r2 = r1.uid == r2.uid

instance Hashable Relay where
    hashWithSalt sault r = hashWithSalt sault r.uid

makeRelay :: Word64 -> WebSocketConnection -> Relay
makeRelay uid connection =
    let
        receiveMessage = parseMessage <$> connection.receiveMessage
        -- sendMessage message = connection.sendMessages [toStrict $ toByteString message.peer, message.content]
        sendMessage = connection.sendMessage . serializeMessage
        close = connection.close $ "Close relay" <> show uid
     in
        Relay{..}
