module Reacthome.Daemon.App where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import Data.Text.Lazy (show)
import Data.Text.Lazy.Encoding
import Data.UUID (UUID)
import Network.WebSockets.Client (ClientApp)
import Reacthome.Relay.Client (RelayClient, makeRelayClient, send, start)
import Prelude hiding (show)

application :: UUID -> ClientApp ()
application peer connection = do
    client <- makeRelayClient connection
    void $ forkIO client.start
    loop peer client 0

loop :: UUID -> RelayClient -> Int -> IO ()
loop peer client count = do
    threadDelay 10_000
    client.send peer $ encodeUtf8 $ "Hello Relay! " <> show count
    loop peer client $ count + 1
