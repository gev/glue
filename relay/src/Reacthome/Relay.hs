module Reacthome.Relay where

import Data.Hashable (Hashable, hashWithSalt)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Network.WebSockets (Connection)

data Relay = Relay
    { uid :: UUID
    , connection :: Connection
    }

instance Eq Relay where
    r1 == r2 = r1.uid == r2.uid

instance Hashable Relay where
    hashWithSalt sault r = hashWithSalt sault r.uid

makeRelay :: Connection -> IO Relay
makeRelay connection = do
    uid <- nextRandom
    pure $ Relay uid connection
