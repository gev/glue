module Reacthome.Auth.Domain.Client.Name where

import Control.Monad.Trans.Except
import Data.Text as Text

newtype ClientName = ClientName {value :: Text}
    deriving stock (Show)
    deriving newtype (Eq)

makeClientName :: (Monad m) => Text -> ExceptT String m ClientName
makeClientName name =
    if isValidClientName name'
        then pure $ ClientName name'
        else throwE "Name should be between 3 and 64 characters"
  where
    name' = Text.strip name

isValidClientName :: Text -> Bool
isValidClientName name =
    length' >= 3 && length' <= 64
  where
    length' = Text.length name
