module WebSockets.Client where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (race_)
import Control.Exception (catch, handle)
import Control.Exception.Base (IOException)
import Control.Monad (void)
import Data.Foldable (for_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Network.WebSockets (HandshakeException, runClient)
import System.Random (newStdGen, uniformR)
import WebSockets.Connection (WebSocketConnection (..), WebSocketSink, WebSocketSource, makeWebSocketConnection)
import WebSockets.Error (WebSocketError (..))
import WebSockets.Options (WebSocketOptions (..))

newtype WebSocketClient = WebSocketClient
    { isConnected :: IO Bool
    }

runWebSocketClient ::
    ( ?options :: WebSocketOptions
    , ?sink :: WebSocketSink
    , ?source :: WebSocketSource
    ) =>
    String -> Int -> String -> IO WebSocketClient
runWebSocketClient host port path = do
    connected <- newIORef False

    let
        isConnected = readIORef connected

        reconnectionLoop delay gen
            | delay > 8 = reconnectionLoop 8 gen
            | otherwise = do
                (e, actualDelay) <- catch @IOException
                    do
                        catch @HandshakeException
                            do
                                runClient host port path $ application . makeWebSocketConnection
                                pure (Nothing, 1)
                            \e -> pure (Just $ HandshakeError e, delay)
                    \e -> pure (Just $ IOError e, delay)
                writeIORef connected False
                for_ e print
                print @String $ "Reconnect in " <> show actualDelay <> "s"
                let usDelay = actualDelay * 1_000_000
                let (usJitter, gen') = uniformR (0, usDelay `div` 4) gen
                threadDelay $ usDelay + usJitter
                let nextDelay = 2 * actualDelay
                reconnectionLoop nextDelay gen'

        application connection = do
            writeIORef connected True
            race_
                do wrap $ connection.runReceiveMessageLoop ?sink
                do wrap $ connection.runSendMessageLoop ?source

        wrap = handle @WebSocketError print

    void . forkIO $ reconnectionLoop 1 =<< newStdGen
    pure WebSocketClient{..}
