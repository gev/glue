module Reacthome.Relay.Message where

import Data.ByteString qualified as S
import Data.ByteString.Lazy qualified as L
import Data.Int (Int64)
import Prelude hiding (length, splitAt, tail, take)

data RelayMessage = RelayMessage
    { to :: !Uid
    , from :: !Uid
    , content :: !S.ByteString
    }

type Uid = S.ByteString
type StrictRaw = S.ByteString
type LazyRaw = L.ByteString

serializeMessage :: RelayMessage -> LazyRaw
serializeMessage message =
    L.fromChunks
        [ message.to
        , message.from
        , message.content
        ]
{-# INLINEABLE serializeMessage #-}

getMessageDestination :: LazyRaw -> Uid
getMessageDestination :: LazyRaw -> Uid =
    L.toStrict . L.take 16
{-# INLINEABLE getMessageDestination #-}

isMessageValid :: LazyRaw -> Bool
isMessageValid = (> headerLength) . L.length
{-# INLINEABLE isMessageValid #-}

headerLength :: Int64
headerLength = 32
