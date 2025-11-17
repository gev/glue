module Reacthome.Relay.Client where

import Control.Concurrent (forkIO)
import Control.Monad (forever, void, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.UUID (UUID)
import Reacthome.Relay.Message (RelayMessage (..))
import Reacthome.Relay.Relay (Relay (..), makeRelay)
import Web.WebSockets.Connection (WebSocketConnection)
import Prelude hiding (length, splitAt, tail)

data RelayClient = RelayClient
    { start :: IO ()
    , send :: RelayMessage -> IO ()
    }

makeRelayClient :: UUID -> WebSocketConnection -> RelayClient
makeRelayClient uid connection =
    let
        start = do
            counter <- newIORef @Int 0
            void $ forkIO $ forever do
                message <- relay.receiveMessage
                counter' <- readIORef counter
                writeIORef counter $ counter' + 1
                when (counter' `mod` 1_000 == 0) do
                    print $ show message

        send = relay.sendMessage
     in
        RelayClient{..}
  where
    relay = makeRelay uid connection
