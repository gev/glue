module WebSockets.Client where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (race)
import Control.Monad (void)
import Data.IORef (newIORef, readIORef, writeIORef)
import Network.WebSockets (runClient)
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
                !res <- runClient host port path $ application . makeWebSocketConnection
                let actualDelay = case res of
                        HandshakeError _ -> 1
                        _ -> delay
                print res
                writeIORef connected False
                print @String $ "Reconnect in " <> show actualDelay <> "s"
                let usDelay = actualDelay * 1_000_000
                let (usJitter, gen') = uniformR (0, usDelay `div` 4) gen
                threadDelay $ usDelay + usJitter
                let nextDelay = 2 * actualDelay
                reconnectionLoop nextDelay gen'

        application connection = do
            writeIORef connected True
            either id id <$> race
                do connection.runReceiveMessageLoop ?sink
                do connection.runSendMessageLoop ?source

    void . forkIO $ reconnectionLoop 1 =<< newStdGen
    pure WebSocketClient{..}
