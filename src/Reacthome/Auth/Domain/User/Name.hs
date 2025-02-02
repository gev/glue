module Reacthome.Auth.Domain.User.Name where

import Control.Monad.Trans.Except
import Data.Text as Text

newtype UserName = UserName {value :: Text}
  deriving stock (Show)
  deriving newtype (Eq)

mkUserName :: (Monad m) => Text -> ExceptT String m UserName
mkUserName name =
  if isValidUserName name'
    then pure $ UserName name'
    else throwE "Name should be between 3 and 64 characters"
 where
  name' = Text.strip name

isValidUserName :: Text -> Bool
isValidUserName name =
  length' >= 3 && length' <= 64
 where
  length' = Text.length name
