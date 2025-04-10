module Reacthome.Auth.Domain.Client.Id where

import Data.Hashable
import Data.UUID
import Data.UUID.V4

newtype ClientId = ClientId {value :: UUID}
    deriving stock (Show)
    deriving newtype (Eq, Hashable)

makeClientId :: UUID -> ClientId
makeClientId = ClientId

makeRandomClientId :: IO ClientId
makeRandomClientId = ClientId <$> nextRandom
