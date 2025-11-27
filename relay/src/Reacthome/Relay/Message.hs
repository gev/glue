module Reacthome.Relay.Message where

import Data.ByteString (concat, length, take)
import Reacthome.Relay (StrictRaw, Uid)
import Prelude hiding (concat, length, splitAt, tail, take)

data RelayMessage = RelayMessage
    { to :: !Uid
    , from :: !Uid
    , content :: !StrictRaw
    }

serializeMessage :: RelayMessage -> StrictRaw
serializeMessage message =
    concat
        [ message.to
        , message.from
        , message.content
        ]
{-# INLINE serializeMessage #-}

getMessageDestination :: StrictRaw -> Uid
getMessageDestination = take 16
{-# INLINE getMessageDestination #-}

isMessageValid :: StrictRaw -> Bool
isMessageValid = (> headerLength) . length
{-# INLINE isMessageValid #-}

headerLength :: Int
headerLength = 32
