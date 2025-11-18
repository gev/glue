module Reacthome.Relay.Message where

import Control.Exception (throw)
import Data.ByteString (ByteString, fromStrict, length, splitAt)
import Data.ByteString.Lazy (toStrict)
import Data.UUID (UUID, fromByteString, toByteString)
import Reacthome.Relay.Error (RelayError (InvalidMessageLength, InvalidUUID))
import Prelude hiding (length, splitAt, tail)

data RelayMessage = RelayMessage
    { peer :: UUID
    , content :: ByteString
    }
    deriving (Show)

serializeMessage :: RelayMessage -> ByteString
serializeMessage message =
    toStrict (toByteString message.peer) <> message.content

parseMessage :: ByteString -> RelayMessage
parseMessage message = do
    let messageLength = length message
    if messageLength < minimumMessageLength
        then
            throw $
                InvalidMessageLength
                    messageLength
                    minimumMessageLength
        else do
            let (peer', content) = splitAt headerLength message
            maybe
                do
                    throw $ InvalidUUID peer'
                do
                    flip RelayMessage content
                do
                    fromByteString $ fromStrict peer'

headerLength :: Int
headerLength = 16

minimumMessageLength :: Int
minimumMessageLength = headerLength + 1
