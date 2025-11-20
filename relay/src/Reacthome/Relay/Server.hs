module Reacthome.Relay.Server where

import Control.Exception (catch)
import Control.Monad (forever)
import Data.ByteString (toStrict)
import Data.Foldable (for_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.UUID (UUID, toByteString)
import Reacthome.Relay.Error (RelayError (..), logError)
import Reacthome.Relay.Message (RelayMessage (..), parseMessage, serializeMessage)
import Reacthome.Relay.Relay (Relay (..))
import Reacthome.Relay.Repository (add, get, makeRelayRepository, remove)
import Web.WebSockets.Connection (WebSocketConnection (..))
import Web.WebSockets.Error (WebSocketError)
import Web.WebSockets.PendingConnection (WebSocketPendingConnection (..))
import Prelude hiding (length, splitAt, tail, take)

newtype RelayServer = RelayServer
    { accept :: WebSocketPendingConnection -> UUID -> IO ()
    }

makeRelayServer :: IO RelayServer
makeRelayServer = do
    repository <- makeRelayRepository
    uid <- newIORef 0
    let
        accept pending peer = do
            let from = toStrict $ toByteString peer
            catch @WebSocketError
                do run from =<< pending.accept
                do logError . WebSocketError from

        run from connection = do
            uid' <- readIORef uid
            let relay = Relay uid' connection
            writeIORef uid $ uid' + 1
            repository.add from relay
            catch @WebSocketError
                do
                    forever do
                        message <- relay.connection.receiveMessage
                        catch @RelayError
                            do
                                let message' = parseMessage message
                                send
                                    message'.peer
                                    RelayMessage
                                        { peer = from
                                        , content = message'.content
                                        }
                            logError
                \e -> do
                    repository.remove from relay
                    logError $ WebSocketError from e

        send to message = do
            relays <- repository.get to
            if null relays
                then logError $ NoPeersFound to
                else do
                    let message' = serializeMessage message
                    for_ relays \relay -> do
                        catch @WebSocketError
                            do
                                relay.connection.sendMessage message'
                            \e -> do
                                repository.remove to relay
                                logError $ WebSocketError to e

    pure RelayServer{..}

headerLength :: Int
headerLength = 16
