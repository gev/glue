module Reacthome.Auth.Domain.UserStatus where

data UserStatus
  = Active
  | Inactive
  | Suspended
  deriving stock (Show, Eq)
