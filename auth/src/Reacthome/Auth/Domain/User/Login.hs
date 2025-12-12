module Reacthome.Auth.Domain.User.Login where

import Data.Char
import Data.Hashable
import Data.Text as Text

newtype UserLogin = UserLogin {value :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Hashable)

makeUserLogin :: Text -> Either String UserLogin
makeUserLogin login =
  if isValidUserLogin login'
    then Right (UserLogin login')
    else Left "Login should be between 3 and 24 characters, starts with a letter and contain only letters, digits, '-', '_', '.', '@'"
 where
  login' = Text.strip login

isValidUserLogin :: Text -> Bool
isValidUserLogin login =
  length' >= 3
    && length' <= 24
    && isLetter (Text.head login)
    && Text.all isValidChar login
 where
  length' = Text.length login
  isValidChar c =
    isLetter c
      || isDigit c
      || c == '-'
      || c == '_'
      || c == '.'
      || c == '@'
