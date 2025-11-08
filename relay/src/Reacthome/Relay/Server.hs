module Reacthome.Relay.Server where

import Control.Exception (SomeException, catch, finally, throwIO)
import Control.Monad (forever, when)
import Data.Foldable (for_)
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (UUID)
import Network.WebSockets (
    PendingConnection,
    acceptRequestWith,
    defaultAcceptRequest,
    defaultPingPongOptions,
    sendClose,
    withPingPong,
 )
import Reacthome.Relay.Connection (connection, makeRelayConnection)
import Reacthome.Relay.Error (RelayError (..), logError)
import Reacthome.Relay.Message (RelayMessage (..), receiveMessage, sendMessage)
import Reacthome.Relay.Repository (add, get, makeRepository, remove)
import Prelude hiding (length, splitAt, tail)

newtype RelayServer = RelayServer
    { connect :: PendingConnection -> UUID -> IO ()
    }

makeRelayServer :: IO RelayServer
makeRelayServer = do
    repository <- makeRepository

    let
        connect pending peer =
            catch @SomeException
                do
                    connection <- acceptRequestWith pending defaultAcceptRequest
                    withPingPong defaultPingPongOptions connection $ run peer
                do
                    logError . ConnectionError peer

        run peer connection = do
            relay <- makeRelayConnection connection
            repository.add peer relay
            finally
                do
                    forever $ loop peer relay
                do
                    close peer relay

        loop peer relay =
            catch @RelayError
                do
                    catch @SomeException
                        do
                            message <- receiveMessage relay.connection
                            handle peer message
                        do
                            throwIO . ReceiveError peer
                \e -> do
                    logError e
                    close peer relay

        handle from message = do
            relays <- repository.get message.peer

            when (null relays) do
                throwIO $ NoRelaysFound message.peer

            let to = message.peer
            for_ relays \relay ->
                catch @SomeException
                    do
                        sendMessage relay.connection $ RelayMessage from message.content
                    \e -> do
                        logError $ SendError to e
                        close to relay

        close peer relay = do
            catch @SomeException
                do
                    sendClose relay.connection $ encodeUtf8 "Close Relay"
                do
                    logError . CloseError peer
            repository.remove peer relay

    pure RelayServer{..}
