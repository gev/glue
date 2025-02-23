module JWT where

import JWT.Header
import JWT.Payload

data Token = Token
    { header :: Header
    , payload :: Payload
    }
    deriving stock (Show)
