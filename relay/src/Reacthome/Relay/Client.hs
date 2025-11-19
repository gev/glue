module Reacthome.Relay.Client where

import Control.Concurrent (forkIO)
import Control.Monad (forever, void, when)
import Data.ByteString.Lazy (ByteString)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Word (Word64)
import Web.WebSockets.Connection (WebSocketConnection (..))
import Prelude hiding (length, splitAt, tail)

data RelayClient = RelayClient
    { start :: IO ()
    , send :: ByteString -> IO ()
    }

makeRelayClient :: WebSocketConnection -> RelayClient
makeRelayClient connection =
    let
        start = do
            counter <- newIORef @Word64 0
            void $ forkIO $ forever do
                message <- connection.receiveMessage
                counter' <- readIORef counter
                writeIORef counter $ counter' + 1
                when (counter' `mod` 1_000 == 0) do
                    print $ show message

        send = connection.sendMessage
     in
        RelayClient{..}
