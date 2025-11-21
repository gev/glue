module Web.WebSockets.Connection where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (catch, throwIO)
import Control.Monad (forever, void)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.WebSockets (
    Connection,
    ConnectionException,
    receiveData,
    sendBinaryDatas,
    sendClose,
 )
import Util.Channel (Channel (..), makeQueue)
import Web.WebSockets.Error (WebSocketError (..))

data WebSocketConnection = WebSocketConnection
    { receiveMessage :: IO ByteString
    , sendMessage :: ByteString -> IO ()
    , sendMessages :: [ByteString] -> IO ()
    , close :: Text -> IO ()
    }

makeWebSocketConnection :: Connection -> IO WebSocketConnection
makeWebSocketConnection connection = do
    txQueue <- makeQueue 10_000

    void . forkIO $ forever do
        messages <- txQueue.flush
        catch @ConnectionException
            do sendBinaryDatas connection messages
            do throwIO . SendError
        threadDelay 1_000

    let
        receiveMessage =
            catch @ConnectionException
                do receiveData connection
                do throwIO . ReceiveError

        sendMessage = txQueue.write

        sendMessages = traverse_ sendMessage

        close message =
            catch @ConnectionException
                do sendClose connection $ encodeUtf8 message
                do throwIO . CloseError

    pure WebSocketConnection{..}
