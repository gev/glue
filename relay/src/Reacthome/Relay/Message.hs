module Reacthome.Relay.Message where

import Control.Exception (throw)
import Data.ByteString.Lazy (ByteString, length, splitAt)
import Data.Int (Int64)
import Data.UUID (UUID, fromByteString, toByteString)
import Network.WebSockets (Connection, receiveData, sendBinaryData)
import Reacthome.Relay.Error (RelayError (InvalidMessageLength, InvalidUUID))
import Prelude hiding (length, splitAt, tail)

data RelayMessage = RelayMessage
    { peer :: UUID
    , content :: ByteString
    }

serializeMessage :: RelayMessage -> ByteString
serializeMessage message =
    toByteString message.peer <> message.content

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
                    fromByteString peer'

receiveMessage :: Connection -> IO RelayMessage
receiveMessage connection =
    parseMessage <$> receiveData connection

sendMessage :: Connection -> RelayMessage -> IO ()
sendMessage connection =
    sendBinaryData connection . serializeMessage

headerLength :: Int64
headerLength = 16

minimumMessageLength :: Int64
minimumMessageLength = headerLength + 1
