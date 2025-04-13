module Reacthome.Assist.Domain.Query where

import Data.Aeson
import Data.Text
import GHC.Generics

data Query = Query
    { user_agent :: Text
    , skill :: Text
    , skill_user :: Text
    , skill_application :: Text
    , session :: Text
    , message :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)
