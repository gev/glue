module Reacthome.Relay.Server where

import Control.Exception (catch, finally, throwIO)
import Control.Monad (forever)
import Data.Foldable (for_)
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (UUID)
import Network.WebSockets (
    ConnectionException,
    HandshakeException,
    PendingConnection,
    acceptRequestWith,
    defaultAcceptRequest,
    defaultPingPongOptions,
    receiveData,
    sendBinaryData,
    sendClose,
    withPingPong,
 )
import Reacthome.Relay.Connection (connection, makeRelayConnection)
import Reacthome.Relay.Error (RelayError (..), logError)
import Reacthome.Relay.Message (RelayMessage (..), parseMessage, serializeMessage)
import Reacthome.Relay.Repository (add, get, makeRepository, remove)
import Prelude hiding (length, splitAt, tail)

newtype RelayServer = RelayServer
    { start :: PendingConnection -> UUID -> IO ()
    }

makeRelayServer :: IO RelayServer
makeRelayServer = do
    repository <- makeRepository

    let
        start pending peer =
            catch @HandshakeException
                do
                    connection <- acceptRequestWith pending defaultAcceptRequest
                    withPingPong defaultPingPongOptions connection $ run peer
                do
                    logError . HandshakeError peer

        run peer connection = do
            relay <- makeRelayConnection connection
            repository.add peer relay
            catch @RelayError
                do
                    finally
                        do forever $ loop peer relay
                        do close peer relay
                logError

        loop peer relay = do
            message <- receiveMessage peer relay
            handle peer message

        handle from message = do
            relays <- repository.get message.peer
            if null relays
                then logError $ NoPeersFound message.peer
                else for_ relays \relay ->
                    catch @RelayError
                        do
                            sendMessage relay $
                                RelayMessage
                                    { peer = from
                                    , content = message.content
                                    }
                        \e -> do
                            logError e
                            close message.peer relay

        receiveMessage peer relay =
            parseMessage <$> catch @ConnectionException
                do receiveData relay.connection
                do throwIO . ReceiveError peer

        sendMessage relay message =
            catch @ConnectionException
                do sendBinaryData relay.connection $ serializeMessage message
                do throwIO . SendError message.peer

        close peer relay = do
            catch @ConnectionException
                do sendClose relay.connection $ encodeUtf8 "Close Relay"
                do logError . CloseError peer
            repository.remove peer relay

    pure RelayServer{..}
