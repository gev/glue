module Reacthome.Auth.Environment where

import Data.Text
import Database.SQLite.Simple

data Environment = Environment
    { name :: Text
    , domain :: Text
    , authTimeout :: Int
    , authFlowCookieTTL :: Int
    , authCodeTTL :: Int
    , challengeSize :: Int
    , accessTokenTTL :: Int
    , db :: Connection
    }
