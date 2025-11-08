module Reacthome.Relay.Connection where

import Data.Hashable (Hashable, hashWithSalt)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Network.WebSockets (Connection)

data RelayConnection = RelayConnection
    { uid :: UUID
    , connection :: Connection
    }

instance Eq RelayConnection where
    r1 == r2 = r1.uid == r2.uid

instance Hashable RelayConnection where
    hashWithSalt sault r = hashWithSalt sault r.uid

makeRelayConnection :: Connection -> IO RelayConnection
makeRelayConnection connection = do
    uid <- nextRandom
    pure $ RelayConnection uid connection
