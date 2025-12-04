module Web.WebSockets.Client where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.Chan.Unagi.Bounded (Element (tryRead), newChan, readChan, tryReadChan, writeChan)
import Control.Exception (catch, handle)
import Control.Exception.Base (IOException)
import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Vector (toList, unsafeFreeze, unsafeTake)
import Data.Vector.Mutable (unsafeNew, unsafeWrite)
import Network.WebSockets (HandshakeException, runClient)
import System.Random (newStdGen, uniformR)
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
        chunkSize = ?options.chunkSize
        isConnected = readIORef connected
        receiveMessage = readChan outSource
        sendMessage = writeChan inSink

        reconnectionLoop delay gen
            | delay > 8 = reconnectionLoop 8 gen
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
                let usDelay = actualDelay * 1_000_000
                let (usJitter, gen') = uniformR (0, usDelay `div` 4) gen
                threadDelay $ usDelay + usJitter
                let nextDelay = 2 * actualDelay
                reconnectionLoop nextDelay gen'

        application connection = do
            writeIORef connected True
            race_
                do wrap $ runRx connection
                do wrap $ runTx connection

        runRx connection = forever do
            !message <- connection.receiveMessage
            writeChan inSource message

        runTx connection = forever do
            let
                processMessageLoop !vector !index = do
                    (!el, !waitMessage) <- tryReadChan outSink
                    !maybeMessage <- tryRead el
                    case maybeMessage of
                        Nothing -> do
                            !actualVector <-
                                if index > 0
                                    then sendBatch vector index
                                    else pure vector
                            threadDelay ?options.delay
                            !message <- waitMessage
                            unsafeWrite actualVector 0 message
                            processMessageLoop actualVector 1
                        Just !message -> do
                            unsafeWrite vector index message
                            let nextIndex = index + 1
                            if nextIndex < chunkSize
                                then do
                                    processMessageLoop vector nextIndex
                                else do
                                    newVector <- sendBatch vector chunkSize
                                    processMessageLoop newVector 0

                sendBatch !vector !size = do
                    !frozen <- unsafeFreeze vector
                    let !messages = toList $ unsafeTake size frozen
                    void $ connection.sendMessages messages
                    unsafeNew chunkSize

            initialVector <- unsafeNew chunkSize
            processMessageLoop initialVector 0

        wrap = handle @WebSocketError print

    void . forkIO $ reconnectionLoop 1 =<< newStdGen
    pure WebSocketClient{..}
