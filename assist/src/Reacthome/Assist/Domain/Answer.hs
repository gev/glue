module Reacthome.Assist.Domain.Answer where

import Data.Text

data Answer = Answer
    { message :: Text
    , sessionId :: Text
    , endSession :: Bool
    }
    deriving stock (Show)
