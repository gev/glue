module WebSockets.Client where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception (try)
import Control.Exception.Base (IOException)
import Control.Monad (void)
import Data.IORef (newIORef, readIORef, writeIORef)
import Network.WebSockets (HandshakeException, runClient)
import System.Random (newStdGen, uniformR)
import WebSockets.Connection (WebSocketConnection (..), WebSocketSink, WebSocketSource, makeWebSocketConnection)
import WebSockets.Error (WebSocketError (..), joinExceptions)
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
                !err <- run
                let actualDelay = getActualDelay err delay
                print err
                writeIORef connected False
                print @String $ "Reconnect in " <> show actualDelay <> "s"
                let usDelay = actualDelay * 1_000_000
                let (usJitter, gen') = uniformR (0, usDelay `div` 4) gen
                threadDelay $ usDelay + usJitter
                let nextDelay = 2 * actualDelay
                reconnectionLoop nextDelay gen'

        run =
            selectError . joinExceptions HandshakeError
                <$> try @IOException do
                    try @HandshakeException do
                        runClient host port path $ application . makeWebSocketConnection

        application connection = do
            writeIORef connected True
            selectError <$> race
                do connection.runReceiveMessageLoop ?sink
                do connection.runSendMessageLoop ?source

        getActualDelay (HandshakeError _) _ = 1
        getActualDelay _ delay = delay

        selectError = either id id

    void . forkIO $ reconnectionLoop 1 =<< newStdGen
    pure WebSocketClient{..}
