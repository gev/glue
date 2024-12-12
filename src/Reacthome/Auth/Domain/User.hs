module Reacthome.Auth.Domain.User (
  User,

  -- * User Fields
  uid,
  login,
  passwordHash,
  status,
  createdAt,
  updatedAt,

  -- * User Smart Constructors
  mkUser,

  -- * User Modifiers
  changeUserLogin,
  changeUserPassword,
  changeUserStatus,

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

import Data.Time (UTCTime)
import Reacthome.Auth.Domain.UserId (UserId)
import Reacthome.Auth.Domain.UserLogin (UserLogin)
import Reacthome.Auth.Domain.UserPassword (UserPassword)
import Reacthome.Auth.Domain.UserStatus (UserStatus (..))

data User = User
  { uid :: UserId
  , login :: UserLogin
  , passwordHash :: UserPassword
  , status :: UserStatus
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Eq, Show)

mkUser ::
  UserId ->
  UserLogin ->
  UserPassword ->
  UTCTime ->
  User
mkUser uid login passwordHash createdAt =
  User
    { uid = uid
    , login = login
    , passwordHash = passwordHash
    , status = Active
    , createdAt = createdAt
    , updatedAt = createdAt
    }

changeUserLogin ::
  User ->
  UserLogin ->
  UTCTime ->
  User
changeUserLogin user newLogin updatedAt =
  user
    { login = newLogin
    , updatedAt = updatedAt
    }

changeUserPassword ::
  User ->
  UserPassword ->
  UTCTime ->
  User
changeUserPassword user newPasswordHash updatedAt =
  user
    { passwordHash = newPasswordHash
    , updatedAt = updatedAt
    }

changeUserStatus ::
  User ->
  UserStatus ->
  UTCTime ->
  User
changeUserStatus user newStatus updatedAt =
  user
    { status = newStatus
    , updatedAt = updatedAt
    }

isUserActive :: User -> Bool
isUserActive user = status user == Active

isUserInactive :: User -> Bool
isUserInactive user = status user == Inactive

isUserSuspended :: User -> Bool
isUserSuspended user = status user == Suspended
