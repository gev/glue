module Reacthome.Auth.Domain.UserPassword (
  UserPassword,
  mkUserPassword,
  isValidUserPassword,
) where

import Data.Char (isDigit, isLetter, isPunctuation, isSymbol)
import Data.Text as Text (Text, all, length, null)

import Validate (mkValid)

newtype UserPassword = UserPassword Text
  deriving (Show, Eq)

mkUserPassword :: Text -> Maybe UserPassword
mkUserPassword = mkValid isValidUserPassword UserPassword

isValidUserPassword :: Text -> Bool
isValidUserPassword password =
  ( not (Text.null password)
      && Text.length password >= 8
      && Text.length password <= 24
      && Text.all isValidChar password
  )
 where
  isValidChar c =
    isLetter c
      || isDigit c
      || isSymbol c
      || isPunctuation c
