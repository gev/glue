module Reacthome.Auth.Domain.User (
  User,

  -- * User Fields
  userId,
  userLogin,
  userPasswordHash,
  userStatus,
  userCreatedAt,
  userUpdatedAt,

  -- * User Smart Constructors
  mkUser,
  createActiveUser,
  changeUserLogin,
  changeUserPassword,
  changeUserStatus,

  -- * User Status Checks
  isUserActive,
  isUserInactive,

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
  { userId :: UserId
  , userLogin :: UserLogin
  , userPasswordHash :: UserPassword
  , userStatus :: UserStatus
  , userCreatedAt :: UTCTime
  , userUpdatedAt :: UTCTime
  }
  deriving (Eq, Show)

mkUser ::
  UserId ->
  UserLogin ->
  UserPassword ->
  UTCTime ->
  User
mkUser userId userLogin userPasswordHash createdAt =
  User
    { userId = userId
    , userLogin = userLogin
    , userPasswordHash = userPasswordHash
    , userStatus = Inactive -- Default to Inactive
    , userCreatedAt = createdAt
    , userUpdatedAt = createdAt
    }

createActiveUser ::
  UserId ->
  UserLogin ->
  UserPassword ->
  UTCTime ->
  User
createActiveUser userId userLogin userPasswordHash createdAt =
  (mkUser userId userLogin userPasswordHash createdAt)
    { userStatus = Active
    }

changeUserLogin ::
  User ->
  UserLogin ->
  UTCTime ->
  User
changeUserLogin user newLogin updatedAt =
  user
    { userLogin = newLogin
    , userUpdatedAt = updatedAt
    }

changeUserPassword ::
  User ->
  UserPassword ->
  UTCTime ->
  User
changeUserPassword user newPasswordHash updatedAt =
  user
    { userPasswordHash = newPasswordHash
    , userUpdatedAt = updatedAt
    }

changeUserStatus ::
  User ->
  UserStatus ->
  UTCTime ->
  User
changeUserStatus user newStatus updatedAt =
  user
    { userStatus = newStatus
    , userUpdatedAt = updatedAt
    }

isUserActive :: User -> Bool
isUserActive user = userStatus user == Active

isUserInactive :: User -> Bool
isUserInactive user = userStatus user == Inactive
