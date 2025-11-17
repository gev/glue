module Web.WebSockets.Connection where

import Control.Exception (catch, throwIO)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.WebSockets (
    Connection,
    ConnectionException,
    receiveData,
    sendBinaryData,
    sendClose,
 )
import Web.WebSockets.Error (WebSocketError (..))

data WebSocketConnection = WebSocketConnection
    { receiveMessage :: IO ByteString
    , sendMessage :: ByteString -> IO ()
    , close :: Text -> IO ()
    }

makeWebSocketConnection :: Connection -> WebSocketConnection
makeWebSocketConnection connection =
    WebSocketConnection{receiveMessage, sendMessage, close}
  where
    receiveMessage =
        catch @ConnectionException
            do receiveData connection
            do throwIO . ReceiveError

    sendMessage message =
        catch @ConnectionException
            do sendBinaryData connection message
            do throwIO . SendError

    close message =
        catch @ConnectionException
            do sendClose connection $ encodeUtf8 message
            do throwIO . CloseError
