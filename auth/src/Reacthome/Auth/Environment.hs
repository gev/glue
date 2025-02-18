module Reacthome.Auth.Environment where

import Data.Text

data Environment = Environment
    { name :: Text
    , domain :: Text
    , authTimeout :: Int
    , authFlowCookieTTL :: Int
    , authCodeTTL :: Int
    , challengeSize :: Int
    }
