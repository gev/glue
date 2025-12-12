module Reacthome.Auth.Domain.User.Name where

import Data.Text as Text

newtype UserName = UserName {value :: Text}
  deriving stock (Show)
  deriving newtype (Eq)

makeUserName :: Text -> Either String UserName
makeUserName name =
  if isValidUserName name'
    then Right (UserName name')
    else Left "Name should be between 3 and 64 characters"
 where
  name' = Text.strip name

isValidUserName :: Text -> Bool
isValidUserName name =
  length' >= 3 && length' <= 64
 where
  length' = Text.length name
