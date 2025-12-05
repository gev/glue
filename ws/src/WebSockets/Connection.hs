module WebSockets.Connection where

import Control.Concurrent (threadDelay)
import Control.Exception (catch, throwIO)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (toList, unsafeFreeze, unsafeTake)
import Data.Vector.Mutable (unsafeNew, unsafeWrite)
import Network.WebSockets (ConnectionException)
import Network.WebSockets.Connection (Connection, receiveData, sendBinaryDatas, sendClose)
import WebSockets.Error (WebSocketError (..))
import WebSockets.Options (WebSocketOptions (..))

type WebSocketSink = ByteString -> IO ()
type WebSocketSource = IO (Maybe ByteString, IO ByteString)

data WebSocketConnection = WebSocketConnection
    { runReceiveMessageLoop :: WebSocketSink -> IO ()
    , runSendMessageLoop :: WebSocketSource -> IO ()
    , close :: Text -> IO ()
    }

makeWebSocketConnection ::
    (?options :: WebSocketOptions) =>
    Connection -> WebSocketConnection
makeWebSocketConnection connection =
    let
        receiveMessage =
            catch @ConnectionException
                do
                    receiveData connection
                do throwIO . ReceiveError

        sendMessages messages =
            catch @ConnectionException
                do sendBinaryDatas connection messages
                do throwIO . SendError

        runReceiveMessageLoop writeToSink = forever do
            !message <- receiveMessage
            writeToSink message

        runSendMessageLoop tryReadFromSource = do
            let
                processMessageLoop !vector !index = do
                    (!maybeMessage, !waitMessage) <- tryReadFromSource
                    case maybeMessage of
                        Nothing -> do
                            !actualVector <-
                                if index > 0
                                    then sendBatch vector index
                                    else pure vector
                            threadDelay ?options.delay
                            !message <- waitMessage
                            unsafeWrite actualVector 0 message
                            processMessageLoop actualVector 1
                        Just !message -> do
                            unsafeWrite vector index message
                            let nextIndex = index + 1
                            if nextIndex < ?options.chunkSize
                                then do
                                    processMessageLoop vector nextIndex
                                else do
                                    newVector <- sendBatch vector ?options.chunkSize
                                    processMessageLoop newVector 0

                sendBatch !vector !size = do
                    !frozen <- unsafeFreeze vector
                    let !messages = toList $ unsafeTake size frozen
                    sendMessages messages
                    unsafeNew ?options.chunkSize

            initialVector <- unsafeNew ?options.chunkSize
            processMessageLoop initialVector 0

        close message =
            catch @ConnectionException
                do sendClose connection $ encodeUtf8 message
                do throwIO . CloseError
     in
        WebSocketConnection{..}
