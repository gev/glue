module WebSockets.Connection where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Exception.Base (IOException)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (toList, unsafeFreeze, unsafeTake)
import Data.Vector.Mutable (unsafeNew, unsafeWrite)
import Network.WebSockets (ConnectionException)
import Network.WebSockets.Connection (Connection, receiveData, sendBinaryDatas, sendClose)
import WebSockets.Error (WebSocketError (..), joinExceptions)
import WebSockets.Options (WebSocketOptions (..))

type WebSocketSink = ByteString -> IO ()
type WebSocketSource = IO (Maybe ByteString, IO ByteString)

data WebSocketConnection = WebSocketConnection
    { runReceiveMessageLoop :: WebSocketSink -> IO WebSocketError
    , runSendMessageLoop :: WebSocketSource -> IO WebSocketError
    , close :: Text -> IO ()
    }

makeWebSocketConnection ::
    (?options :: WebSocketOptions) =>
    Connection -> WebSocketConnection
makeWebSocketConnection connection =
    let
        receiveMessage =
            joinExceptions ReceiveError
                <$> try @IOException do
                    try @ConnectionException do
                        receiveData connection

        sendMessages message =
            joinExceptions SendError
                <$> try @IOException do
                    try @ConnectionException do
                        sendBinaryDatas connection message

        close message = void do
            try @IOException do
                try @ConnectionException do
                    sendClose connection $ encodeUtf8 message

        runReceiveMessageLoop writeToSink = processMessageLoop
          where
            processMessageLoop = do
                !successful <- receiveMessage
                case successful of
                    Right message -> do
                        writeToSink message
                        processMessageLoop
                    Left e -> pure e

        runSendMessageLoop tryReadFromSource = do
            initialVector <- unsafeNew ?options.chunkSize
            processChunksOfMessages initialVector 0
          where
            processChunksOfMessages !vector !index = do
                (!maybeMessage, !waitMessage) <- tryReadFromSource
                case maybeMessage of
                    Nothing -> do
                        !successful <-
                            if index > 0
                                then sendChunk vector index
                                else pure $ Right vector
                        case successful of
                            Right actualVector -> do
                                threadDelay ?options.delay
                                !message <- waitMessage
                                unsafeWrite actualVector 0 message
                                processChunksOfMessages actualVector 1
                            Left e -> pure e
                    Just !message -> do
                        unsafeWrite vector index message
                        let nextIndex = index + 1
                        if nextIndex < ?options.chunkSize
                            then do
                                processChunksOfMessages vector nextIndex
                            else do
                                successful <- sendChunk vector ?options.chunkSize
                                case successful of
                                    Right newVector -> do
                                        processChunksOfMessages newVector 0
                                    Left e -> pure e

            sendChunk !vector !size = do
                !frozen <- unsafeFreeze vector
                let !chunk = toList $ unsafeTake size frozen
                successful <- sendMessages chunk
                case successful of
                    Right () -> Right <$> unsafeNew ?options.chunkSize
                    Left e -> pure $ Left e
     in
        WebSocketConnection{..}
