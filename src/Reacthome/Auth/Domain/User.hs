module Reacthome.Auth.Domain.User where

import Reacthome.Auth.Domain.UserId
import Reacthome.Auth.Domain.UserLogin
import Reacthome.Auth.Domain.UserName
import Reacthome.Auth.Domain.UserStatus

data User = User
  { id :: UserId
  , login :: UserLogin
  , name :: UserName
  , status :: UserStatus
  }
  deriving stock (Eq, Show)

mkUser ::
  UserId ->
  UserLogin ->
  UserName ->
  UserStatus ->
  User
mkUser = User

mkNewUser ::
  UserId ->
  UserLogin ->
  UserName ->
  User
mkNewUser uid login name =
  mkUser uid login name Active

changeUserLogin ::
  User ->
  UserLogin ->
  User
changeUserLogin user newLogin =
  user
    { login = newLogin
    }

activateUser :: User -> User
activateUser user =
  user
    { status = Active
    }

inactivateUser :: User -> User
inactivateUser user =
  user
    { status = Inactive
    }

suspendUser :: User -> User
suspendUser user =
  user
    { status = Suspended
    }

isUserActive :: User -> Bool
isUserActive user = user.status == Active

isUserInactive :: User -> Bool
isUserInactive user = user.status == Inactive

isUserSuspended :: User -> Bool
isUserSuspended user = user.status == Suspended
