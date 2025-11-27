module Reacthome.Relay.Message where

import Data.ByteString qualified as S
import Data.ByteString.Lazy qualified as L
import Reacthome.Relay (LazyRaw, StrictRaw, Uid)
import Prelude hiding (length, splitAt, tail, take)

data RelayMessage = RelayMessage
    { to :: !Uid
    , from :: !Uid
    , content :: !StrictRaw
    }

serializeMessage :: RelayMessage -> LazyRaw
serializeMessage message =
    L.fromChunks
        [ message.to
        , message.from
        , message.content
        ]
{-# INLINEABLE serializeMessage #-}

getMessageDestination :: LazyRaw -> Uid
getMessageDestination = S.concat . L.toChunks . L.take 16
{-# INLINEABLE getMessageDestination #-}

isMessageDestinationValid :: StrictRaw -> Bool
isMessageDestinationValid = (== 16) . S.length
{-# INLINEABLE isMessageDestinationValid #-}
