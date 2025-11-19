module Reacthome.Relay.Relay where

import Data.ByteString.Short (ShortByteString)
import Data.Hashable (Hashable, hashWithSalt)
import Data.Text (show)
import Data.Word (Word64)
import Web.WebSockets.Connection (WebSocketConnection (..))
import Prelude hiding (show)

data Relay = Relay
    { uid :: !Word64
    , receiveMessage :: IO ShortByteString
    , sendMessage :: ShortByteString -> IO ()
    , close :: IO ()
    }

instance Eq Relay where
    r1 == r2 = r1.uid == r2.uid

instance Hashable Relay where
    hashWithSalt sault r = hashWithSalt sault r.uid

makeRelay :: Word64 -> WebSocketConnection -> Relay
makeRelay uid connection =
    let
        receiveMessage = connection.receiveMessage
        sendMessage = connection.sendMessage
        close = connection.close $ "Close relay" <> show uid
     in
        Relay{..}
