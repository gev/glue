module Reacthome.Relay.Server where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (catch)
import Control.Monad (forever, void)
import Data.Foldable (for_)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.UUID (UUID)
import Reacthome.Relay.Error (RelayError (..), logError)
import Reacthome.Relay.Message (RelayMessage (..))
import Reacthome.Relay.Relay (Relay (..), newRelay)
import Reacthome.Relay.Repository (add, get, makeRepository, remove)
import Web.WebSockets.Error (WebSocketError)
import Web.WebSockets.PendingConnection (WebSocketPendingConnection (..))
import Prelude hiding (length, splitAt, tail)

newtype RelayServer = RelayServer
    { start :: WebSocketPendingConnection -> UUID -> IO ()
    }

makeRelayServer :: IO RelayServer
makeRelayServer = do
    repository <- makeRepository

    rx <- newIORef @Int 0
    tx <- newIORef @Int 0

    void $ forkIO $ forever do
        rx0 <- readIORef rx
        tx0 <- readIORef tx
        threadDelay 1_000_000
        rx1 <- readIORef rx
        tx1 <- readIORef tx
        print $ "Rx: " <> show rx1 <> " | " <> show (rx1 - rx0) <> " rps"
        print $ "Tx: " <> show tx1 <> " | " <> show (tx1 - tx0) <> " rps"

    let
        start pending peer =
            catch @WebSocketError
                do run peer =<< pending.accept
                do logError . WebSocketError peer

        run peer connection = do
            relay <- newRelay connection
            repository.add peer relay
            catch @WebSocketError
                do
                    forever do
                        message <- relay.receiveMessage
                        modifyIORef rx (+ 1)
                        handle peer message
                \e -> do
                    repository.remove peer relay
                    logError $ WebSocketError peer e

        handle from message = do
            let to = message.peer
            relays <- repository.get to
            if null relays
                then logError $ NoPeersFound to
                else for_ relays \relay ->
                    catch @WebSocketError
                        do
                            relay.sendMessage
                                RelayMessage
                                    { peer = from
                                    , content = message.content
                                    }
                            modifyIORef tx (+ 1)
                        \e -> do
                            repository.remove to relay
                            logError $ WebSocketError to e

    pure RelayServer{..}
