module Reacthome.Auth.Domain.User (
  User,

  -- * User Fields
  uid,
  login,
  passwordHash,
  status,

  -- * User Smart Constructors
  mkUser,
  mkNewUser,

  -- * User Modifiers
  changeUserLogin,
  changeUserPassword,
  activateUser,
  inactivateUser,
  suspendUser,

  -- * User Status Checks
  isUserActive,
  isUserInactive,
  isUserSuspended,

  -- * Reexported Types
  UserId,
  UserLogin,
  UserPassword,
  UserStatus (..),
)
where

import Reacthome.Auth.Domain.UserId (UserId)
import Reacthome.Auth.Domain.UserLogin (UserLogin)
import Reacthome.Auth.Domain.UserPassword (UserPassword)
import Reacthome.Auth.Domain.UserStatus (UserStatus (..))

data User = User
  { uid :: UserId
  , login :: UserLogin
  , passwordHash :: UserPassword
  , status :: UserStatus
  }
  deriving (Eq, Show)

mkUser ::
  UserId ->
  UserLogin ->
  UserPassword ->
  UserStatus ->
  User
mkUser = User

mkNewUser ::
  UserId ->
  UserLogin ->
  UserPassword ->
  User
mkNewUser uid login passwordHash =
  mkUser uid login passwordHash Active

changeUserLogin ::
  User ->
  UserLogin ->
  User
changeUserLogin user newLogin =
  user
    { login = newLogin
    }

changeUserPassword ::
  User ->
  UserPassword ->
  User
changeUserPassword user newPasswordHash =
  user
    { passwordHash = newPasswordHash
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
