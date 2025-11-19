module Reacthome.Relay.Relay where

import Data.Hashable (Hashable, hashWithSalt)
import Data.Word (Word64)
import Web.WebSockets.Connection (WebSocketConnection (..))
import Prelude hiding (show)

data Relay = Relay
    { uid :: !Word64
    , connection :: !WebSocketConnection
    }

instance Eq Relay where
    r1 == r2 = r1.uid == r2.uid

instance Hashable Relay where
    hashWithSalt sault r = hashWithSalt sault r.uid
