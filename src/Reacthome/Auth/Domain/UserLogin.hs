module Reacthome.Auth.Domain.UserLogin (
  UserLogin,
  mkUserLogin,
  isValidUserLogin,
) where

import Data.Char (isDigit, isLetter)
import Data.Text as Text (Text, all, head, length, null)
import Validate (mkValid)

newtype UserLogin = UserLogin Text
  deriving (Show, Eq)

mkUserLogin :: Text -> Maybe UserLogin
mkUserLogin = mkValid isValidUserLogin UserLogin

isValidUserLogin :: Text -> Bool
isValidUserLogin login =
  ( not (Text.null login)
      && Text.length login >= 3
      && Text.length login <= 24
      && isLetter (Text.head login)
      && Text.all isValidChar login
  )
 where
  isValidChar c =
    isLetter c
      || isDigit c
      || c == '-'
      || c == '_'
      || c == '.'
      || c == '@'
