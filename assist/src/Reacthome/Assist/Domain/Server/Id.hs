module Reacthome.Assist.Domain.Server.Id where

import Data.UUID

newtype ServerId = ServerId {value :: UUID}
    deriving stock (Eq, Show)
