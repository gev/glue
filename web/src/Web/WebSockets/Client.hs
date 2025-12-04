module Web.WebSockets.Client where

import Control.Concurrent (threadDelay)
import Control.Exception (catch)
import Control.Exception.Base (IOException)
import Network.WebSockets (HandshakeException, runClient)
import Web.WebSockets.Connection (WebSocketConnection, makeWebSocketConnection)
import Web.WebSockets.Error (WebSocketError (..))

type WebSocketClientApplication = WebSocketConnection -> IO ()

runWebSocketClient :: String -> Int -> String -> WebSocketClientApplication -> IO ()
runWebSocketClient host port path application = reconnectLoop 1
  where
    reconnectLoop delay
        | delay > 16 = reconnectLoop 1
        | otherwise = do
            catch @IOException
                do
                    catch @HandshakeException
                        do
                            catch @WebSocketError
                                do runClient host port path $ application . makeWebSocketConnection
                                \e -> do
                                    print e
                                    print @String $ "Reconnect in 1s"
                                    threadDelay 1_000_000
                                    reconnectLoop 2
                        do print . HandshakeError
                do print . IOException
            print @String $ "Reconnect in " <> show delay <> "s"
            threadDelay $ delay * 1_000_000
            reconnectLoop $ 2 * delay
