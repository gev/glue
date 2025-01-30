module Reacthome.Auth.Domain.UserName where

import Data.Text as Text

newtype UserName = UserName {value :: Text}
    deriving stock (Show)
    deriving newtype (Eq)

mkUserName :: Text -> Either String UserName
mkUserName name =
    if isValidUserName name'
        then
            Right (UserName name')
        else
            Left "Name should be between 3 and 64 characters"
  where
    name' = Text.strip name

isValidUserName :: Text -> Bool
isValidUserName name =
    length' >= 3 && length' <= 64
  where
    length' = Text.length name
