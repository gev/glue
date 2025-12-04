module Web.WebSockets.Connection where

import Control.Exception (IOException, catch, throwIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.WebSockets (ConnectionException)
import Network.WebSockets.Connection (Connection, receiveData, sendBinaryDatas, sendClose)
import Web.WebSockets.Error (WebSocketError (..))

data WebSocketConnection = WebSocketConnection
    { receiveMessage :: IO ByteString
    , sendMessage :: ByteString -> IO ()
    , sendMessages :: [ByteString] -> IO ()
    , close :: Text -> IO ()
    }

makeWebSocketConnection :: Connection -> WebSocketConnection
makeWebSocketConnection connection =
    let
        receiveMessage =
            catch @IOException
                do
                    catch @ConnectionException
                        do
                            receiveData connection
                        do throwIO . ReceiveError
                do throwIO . IOError

        sendMessage = sendMessages . pure

        sendMessages messages =
            catch @IOException
                do
                    catch @ConnectionException
                        do sendBinaryDatas connection messages
                        do throwIO . SendError
                do throwIO . IOError

        close message =
            catch @IOException
                do
                    catch @ConnectionException
                        do sendClose connection $ encodeUtf8 message
                        do throwIO . CloseError
                do throwIO . IOError
     in
        WebSocketConnection{..}
