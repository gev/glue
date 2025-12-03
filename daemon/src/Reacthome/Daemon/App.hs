{-# OPTIONS_GHC -Wno-unused-imports #-}

module Reacthome.Daemon.App where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.Chan.Unagi (InChan, OutChan, readChan, writeChan)
import Control.Concurrent.Chan.Unagi.Bounded qualified as B
import Control.Exception (handle)
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
    ( ?inChan :: InChan StrictRaw
    , ?outChan :: B.OutChan [StrictRaw]
    ) =>
    UUID -> WebSocketClientApplication
application peer connection = do
    let
        from = toStrict $ toByteString peer

        onError = logError . WebSocketError from
        wrap = handle @WebSocketError onError

        runTx = do
            forever do
                !messages <- B.readChan ?outChan
                connection.sendMessages messages

        runRx = forever do
            !message <- connection.receiveMessage
            writeChan ?inChan message

    race_
        do wrap runTx
        do wrap runRx

    print $ "Peer " <> show peer <> " Disconnected"
