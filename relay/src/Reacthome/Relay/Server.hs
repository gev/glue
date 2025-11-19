module Reacthome.Relay.Server where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (catch)
import Control.Monad (forever, void)
import Data.ByteString (ByteString, take, toStrict)
import Data.Foldable (for_)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.UUID (UUID, toByteString)
import Data.Word (Word64)
import Reacthome.Relay.Error (RelayError (..), logError)
import Reacthome.Relay.Relay (Relay (..), makeRelay)
import Reacthome.Relay.Repository (add, get, makeRepository, remove)
import Web.WebSockets.Connection (WebSocketConnection (..))
import Web.WebSockets.Error (WebSocketError)
import Web.WebSockets.PendingConnection (WebSocketPendingConnection (..))
import Prelude hiding (length, splitAt, tail, take)

newtype RelayServer = RelayServer
    { start :: WebSocketPendingConnection -> UUID -> IO ()
    }

makeRelayServer :: IO RelayServer
makeRelayServer = do
    repository <- makeRepository

    uid <- newIORef 0
    rx <- newIORef @Word64 0
    tx <- newIORef @Word64 0

    void $ forkIO $ forever do
        rx0 <- readIORef rx
        tx0 <- readIORef tx
        threadDelay 1_000_000
        rx1 <- readIORef rx
        tx1 <- readIORef tx
        print $ "Rx: " <> show rx1 <> " | " <> show (rx1 - rx0) <> " rps"
        print $ "Tx: " <> show tx1 <> " | " <> show (tx1 - tx0) <> " rps"

    let
        start pending peer = do
            let from = toStrict $ toByteString peer
            catch @WebSocketError
                do run from =<< pending.accept
                do logError . WebSocketError from

        run from connection = do
            uid' <- readIORef uid
            -- let relay = makeRelay uid' connection
            writeIORef uid $ uid' + 1
            -- repository.add from relay
            catch @WebSocketError
                do
                    forever do
                        message <- connection.receiveMessage
                        modifyIORef rx (+ 1)
                        connection.sendMessage message
                        modifyIORef tx (+ 1)
                -- let to = take 16 message
                -- handle to message
                \e -> do
                    -- repository.remove from relay
                    logError $ WebSocketError from e

    -- handle :: ByteString -> ByteString -> IO ()
    -- handle to message = do
    --     relays <- repository.get to
    --     if null relays
    --         then logError $ NoPeersFound to
    --         else for_ relays \relay ->
    --             catch @WebSocketError
    --                 do
    --                     relay.sendMessage message
    --                     modifyIORef tx (+ 1)
    --                 \e -> do
    --                     repository.remove to relay
    --                     logError $ WebSocketError to e

    pure RelayServer{..}
