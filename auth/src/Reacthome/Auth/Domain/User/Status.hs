module Reacthome.Auth.Domain.User.Status where

data UserStatus
  = Active
  | Inactive
  | Suspended
  deriving stock (Show, Eq)
