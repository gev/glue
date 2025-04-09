module Reacthome.Auth.Domain.User where

import Reacthome.Auth.Domain.User.Id
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.User.Name

data User = User
  { id :: UserId
  , login :: UserLogin
  , name :: UserName
  }
  deriving stock (Eq, Show)

makeUser ::
  UserId ->
  UserLogin ->
  UserName ->
  User
makeUser = User

changeUserLogin ::
  User ->
  UserLogin ->
  User
changeUserLogin user newLogin =
  user
    { login = newLogin
    }
