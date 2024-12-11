module Reacthome.Auth.Domain.UserId (
  UserId,
  mkUserId,
) where

import Data.UUID (UUID)

newtype UserId = UserId UUID
  deriving (Show, Eq)

mkUserId :: UUID -> UserId
mkUserId = UserId
