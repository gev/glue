module Reacthome.Auth.Domain.User.Id where

import Data.Hashable
import Data.UUID
import Data.UUID.V4

newtype UserId = UserId {value :: UUID}
  deriving stock (Show)
  deriving newtype (Eq, Hashable)

mkUserId :: UUID -> UserId
mkUserId = UserId

mkRandomUserId :: IO UserId
mkRandomUserId = UserId <$> nextRandom
