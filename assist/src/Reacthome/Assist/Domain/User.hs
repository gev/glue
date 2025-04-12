module Reacthome.Assist.Domain.User where

import Reacthome.Assist.Domain.Server.Id
import Reacthome.Assist.Domain.User.Id

data User = User
  { id :: UserId
  , servers :: [ServerId]
  }
  deriving stock (Show)

makeUser ::
  UserId ->
  [ServerId] ->
  User
makeUser = User
