module Reacthome.Auth.Domain.User.Id where

import Data.Hashable
import Data.UUID
import Data.UUID.V4

newtype UserId = UserId {value :: UUID}
  deriving stock (Show)
  deriving newtype (Eq, Hashable)

makeUserId :: UUID -> UserId
makeUserId = UserId

makeRandomUserId :: IO UserId
makeRandomUserId = UserId <$> nextRandom
