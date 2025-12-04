module Web.WebSockets.Client where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.Chan.Unagi.Bounded (newChan, readChan, writeChan)
import Control.Exception (catch, handle)
import Control.Exception.Base (IOException)
import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Network.WebSockets (HandshakeException, runClient)
import Web.WebSockets.Connection (WebSocketConnection (..), makeWebSocketConnection)
import Web.WebSockets.Error (WebSocketError (..))
import Web.WebSockets.Options (WebSocketOptions (..))

data WebSocketClient = WebSocketClient
    { isConnected :: IO Bool
    , receiveMessage :: IO ByteString
    , sendMessage :: ByteString -> IO ()
    }

runWebSocketClient ::
    (?options :: WebSocketOptions) =>
    String -> Int -> String -> IO WebSocketClient
runWebSocketClient host port path = do
    connected <- newIORef False
    (inSink, outSink) <- newChan ?options.bound
    (inSource, outSource) <- newChan ?options.bound

    let
        isConnected = readIORef connected
        receiveMessage = readChan outSource
        sendMessage = writeChan inSink

        reconnectionLoop delay
            | delay > 8 = reconnectionLoop 8
            | otherwise = do
                (e, actualDelay) <- catch @IOException
                    do
                        catch @HandshakeException
                            do
                                runClient host port path $ application . makeWebSocketConnection
                                pure (Nothing, 1)
                            \e -> pure (Just . ConnectionError $ HandshakeError e, delay)
                    \e -> pure (Just . ConnectionError $ IOError e, delay)
                writeIORef connected False
                for_ e print
                print @String $ "Reconnect in " <> show actualDelay <> "s"
                threadDelay $ actualDelay * 1_000_000
                reconnectionLoop $ 2 * actualDelay

        application connection = do
            writeIORef connected True
            race_
                do wrap $ runRx connection
                do wrap $ runTx connection

        runRx connection = forever do
            !message <- connection.receiveMessage
            writeChan inSource message

        runTx connection = forever do
            !message <- readChan outSink
            connection.sendMessage message

        wrap = handle @WebSocketError print

    void . forkIO $ reconnectionLoop 1
    pure WebSocketClient{..}
