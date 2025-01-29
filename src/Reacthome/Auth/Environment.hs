module Reacthome.Auth.Environment where

import Data.Text

data Environment = Environment
    { name :: Text
    , domain :: Text
    , timeout :: Int
    , userIdSize :: Int
    , challengeSize :: Int
    }
