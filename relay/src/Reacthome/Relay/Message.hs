{-# LANGUAGE Strict #-}

module Reacthome.Relay.Message where

import Control.Exception (throw)
import Data.ByteString (ByteString, length, splitAt, toStrict)
import Data.ByteString.Lazy qualified as L
import Reacthome.Relay.Error (RelayError (..))
import Prelude hiding (length, splitAt, tail)

data PeerMessage = PeerMessage
    { peer :: !Uid
    , content :: StrictRaw
    }
    deriving (Show)

data RelayMessage = RelayMessage
    { from :: !Uid
    , to :: !Uid
    , content :: !ByteString
    }

type Uid = ByteString
type StrictRaw = ByteString
type LazyRaw = L.ByteString

serializeMessage :: PeerMessage -> LazyRaw
serializeMessage message =
    L.fromChunks [message.peer, message.content]
{-# INLINEABLE serializeMessage #-}

parseMessage :: L.ByteString -> PeerMessage
parseMessage message = do
    let message' = toStrict message
    let messageLength = length message'
    if messageLength > headerLength
        then do
            let (peer, content) = splitAt headerLength message'
            PeerMessage{..}
        else
            throw
                InvalidMessageLength
                    { messageLength
                    , minimumLength = headerLength + 1
                    }
{-# INLINEABLE parseMessage #-}

headerLength :: Int
headerLength = 16
