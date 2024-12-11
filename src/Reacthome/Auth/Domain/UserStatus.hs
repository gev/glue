module Reacthome.Auth.Domain.UserStatus (
  UserStatus (..),
) where

data UserStatus
  = Active
  | Inactive
  | Suspended
  deriving (Show, Eq)
