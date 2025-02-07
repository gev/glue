module Reacthome.Auth.Domain.User where

import Reacthome.Auth.Domain.User.Id
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.User.Name
import Reacthome.Auth.Domain.User.Status

data User = User
  { id :: UserId
  , login :: UserLogin
  , name :: UserName
  , status :: UserStatus
  }
  deriving stock (Eq, Show)

makeUser ::
  UserId ->
  UserLogin ->
  UserName ->
  UserStatus ->
  User
makeUser = User

makeNewUser ::
  UserId ->
  UserLogin ->
  UserName ->
  User
makeNewUser uid login name =
  makeUser uid login name Active

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
