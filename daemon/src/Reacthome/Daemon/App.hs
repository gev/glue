{-# OPTIONS_GHC -Wno-unused-imports #-}

module Reacthome.Daemon.App where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Exception (catch, handle)
import Control.Monad (forever, void)
import Data.ByteString (toStrict)
import Data.Text.Encoding
import Data.UUID (UUID, toByteString)
import Reacthome.Relay (StrictRaw)
import Reacthome.Relay.Error (RelayError (..), logError)
import Reacthome.Relay.Message (RelayMessage (..), serializeMessage)
import Web.WebSockets.Client (WebSocketClientApplication)
import Web.WebSockets.Connection (WebSocketConnection (..))
import Web.WebSockets.Error (WebSocketError)

application ::
    ( ?register :: WebSocketConnection -> IO ()
    , ?onMessage :: StrictRaw -> IO ()
    , ?onError :: WebSocketError -> IO ()
    ) =>
    WebSocketClientApplication
application connection = do
    ?register connection
    catch @WebSocketError
        do
            forever do
                !message <- connection.receiveMessage
                ?onMessage message
        ?onError
