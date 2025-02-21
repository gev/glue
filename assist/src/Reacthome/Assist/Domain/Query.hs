module Reacthome.Assist.Domain.Query where

import Data.Aeson
import Data.Text
import GHC.Generics

data Query = Query
    { message :: Text
    , sessionId :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)
