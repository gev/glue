module Reacthome.Relay.Message where

import Control.Exception (throw)
import Data.ByteString (ByteString, length, splitAt, toStrict)
import Data.ByteString.Lazy qualified as L
import Reacthome.Relay.Error (RelayError (..))
import Prelude hiding (length, splitAt, tail)

data RelayMessage = RelayMessage
    { peer :: !ByteString
    , content :: !ByteString
    }
    deriving (Show)

serializeMessage :: RelayMessage -> L.ByteString
serializeMessage message =
    L.fromChunks [message.peer, message.content]
{-# INLINEABLE serializeMessage #-}

parseMessage :: L.ByteString -> RelayMessage
parseMessage message = do
    let message' = toStrict message
    let messageLength = length message'
    if messageLength > headerLength
        then do
            let (peer, content) = splitAt headerLength message'
            RelayMessage{..}
        else
            throw
                InvalidMessageLength
                    { messageLength
                    , minimumLength = headerLength + 1
                    }
{-# INLINEABLE parseMessage #-}

headerLength :: Int
headerLength = 16
