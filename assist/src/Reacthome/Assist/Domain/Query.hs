module Reacthome.Assist.Domain.Query where

import Data.Text

data Query = Query
    { message :: Text
    , sessionId :: Text
    }
    deriving stock (Show)
