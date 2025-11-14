module Web.WebSockets.Socket where

import Control.Exception (catch, throwIO)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.WebSockets (Connection, ConnectionException, receiveData, sendBinaryData, sendClose)
import Web.WebSockets.Error (WebSocketError (CloseError, ReceiveError, SendError))

data WebSocket = WebSocket
    { sendMessage :: ByteString -> IO ()
    , close :: Text -> IO ()
    }

receiveMessage :: Connection -> IO ByteString
receiveMessage connection =
    catch @ConnectionException
        do receiveData connection
        do throwIO . SendError

makeWebSocket :: Connection -> WebSocket
makeWebSocket connection =
    WebSocket
        { sendMessage
        , close
        }
  where
    sendMessage message =
        catch @ConnectionException
            do sendBinaryData connection message
            do throwIO . ReceiveError

    close message =
        catch @ConnectionException
            do sendClose connection $ encodeUtf8 message
            do throwIO . CloseError
