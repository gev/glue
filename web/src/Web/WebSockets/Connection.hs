module Web.WebSockets.Connection where

import Control.Exception (catch, throwIO)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.WebSockets (
    Connection,
    ConnectionException,
    DataMessage (..),
    Message (..),
    receiveData,
    sendClose,
    sendDataMessages,
 )
import Web.WebSockets.Error (WebSocketError (..))

data WebSocketConnection = WebSocketConnection
    { receiveMessage :: IO ByteString
    , sendMessage :: ByteString -> IO ()
    , sendMessages :: [ByteString] -> IO ()
    , close :: Text -> IO ()
    }

makeWebSocketConnection :: Connection -> WebSocketConnection
makeWebSocketConnection connection =
    WebSocketConnection{receiveMessage, sendMessage, sendMessages, close}
  where
    receiveMessage =
        catch @ConnectionException
            do receiveData connection
            do throwIO . ReceiveError

    sendMessage = sendMessages . pure

    sendMessages messages =
        catch @ConnectionException
            do sendDataMessages connection (Binary <$> messages)
            do throwIO . SendError

    close message =
        catch @ConnectionException
            do sendClose connection $ encodeUtf8 message
            do throwIO . CloseError
