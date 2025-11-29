module Reacthome.Relay.Message where

import Data.ByteString qualified as S
import Reacthome.Relay (StrictRaw, Uid)
import Prelude hiding (concat, length, splitAt, tail, take)

data RelayMessage = RelayMessage
    { to :: !Uid
    , from :: !Uid
    , content :: !StrictRaw
    }

serializeMessage :: RelayMessage -> StrictRaw
serializeMessage message =
    S.concat
        [ message.to
        , message.from
        , message.content
        ]
{-# INLINEABLE serializeMessage #-}

getMessageDestination :: StrictRaw -> Uid
getMessageDestination = S.take 16
{-# INLINEABLE getMessageDestination #-}

isMessageDestinationValid :: StrictRaw -> Bool
isMessageDestinationValid = (== 16) . S.length
{-# INLINEABLE isMessageDestinationValid #-}
